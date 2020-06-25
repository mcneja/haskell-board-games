-- To do:
--  Set up for AI play. Generate a list of potential next moves. Score boards.
--  Will probably have to switch to GLUT to get good mouse mapping and viewport management.
--  Split into a game model and a presentation model?

import Data.Char
import Data.List
import Data.Maybe
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

data Player = PlayerO | PlayerX deriving (Eq, Ord, Show, Enum)
type Board = [Int] -- move locations in order of play
data Game = Game {
    board :: Board,
    mousePos :: Point
    } deriving (Show)

gridSize = 150
lineWidth = 15

main :: IO ()
main = play
    displayMode
    backgroundColor
    framesPerSecond
    newGame
    drawGame
    handleInputEvent
    stepTime
    where
        displayMode = InWindow "Tic Tac Toe" (sizeX, sizeY) (5, 5)
        backgroundColor = white
        framesPerSecond = 100
        sizeX = floor (gridSize * 3 + lineWidth)
        sizeY = floor (gridSize * 3 + lineWidth)

stepTime :: Float -> Game -> Game
stepTime _ = id

newGame :: Game
newGame = Game [] (gridSize * 2, 0)

drawGame :: Game -> Picture
drawGame (Game b p) = Pictures [
    drawWinsFor PlayerX b,
    drawWinsFor PlayerO b,
    backgroundGrid,
    marks b,
    highlightMouseSquare p b ]

handleInputEvent :: Event -> Game -> Game
handleInputEvent (EventKey (Char c) Down _ _) g | isDigit c = tryToMark ((digitToInt c) - 1) g
handleInputEvent (EventKey (Char 'n') Down _ _) g = g { board = [] }
handleInputEvent (EventKey (MouseButton LeftButton) Down _ pos) g = tryToMark (boardIndex pos) g
handleInputEvent (EventMotion pos) g = g { mousePos = pos }
handleInputEvent _ g = g

tryToMark :: Int -> Game -> Game
tryToMark i g@(Game b _) | canMark i b = g { board = b ++ [i] }
tryToMark _ g = g

canMark :: Int -> Board -> Bool
canMark i b = (not $ gameOver b) && i >= 0 && i < 9 && (not $ elem i b)

highlightMouseSquare :: Point -> Board -> Picture
highlightMouseSquare pos b = if i >= 0 && (canMark i b) then Translate x y $ mark else Blank
    where
        i = boardIndex pos
        (x, y) = markPositions!!i
        mark = if (length b) `mod` 2 == 0 then highlightMarkO else highlightMarkX

marks :: Board -> Picture
marks b = Pictures $ zipWith mark pics b
    where
        mark p i = let (x, y) = markPositions!!i in Translate x y $ p
        pics = concat $ repeat [markO, markX]

moves :: Player -> [Int] -> [Int]
moves PlayerO = odds
moves PlayerX = evens

odds (x:xs) = x : evens xs
odds _ = []

evens xs = odds (drop 1 xs)

markPositions :: [Point]
markPositions = [ (x, y) | y <- [-r, 0, r], x <- [-r, 0, r] ] where r = gridSize

gameOver :: Board -> Bool
gameOver b =
    (not $ null $ winsFor PlayerX b) ||
    (not $ null $ winsFor PlayerO b) ||
    (length b) >= 9

winsFor :: Player -> Board -> [Int]
winsFor p b = nub . concat $ filter (and . map (flip elem (moves p b))) winCoords

winCoords :: [ [Int] ]
winCoords = [
    [ 0, 1, 2 ],
    [ 3, 4, 5 ],
    [ 6, 7, 8 ],
    [ 0, 3, 6 ],
    [ 1, 4, 7 ],
    [ 2, 5, 8 ],
    [ 0, 4, 8 ],
    [ 2, 4, 6 ]
    ]

boardIndex :: Point -> Int
boardIndex p = let (x, y) = gridCoords p in
    if
        x >= 0 && x < 3 && y >= 0 && y < 3
    then
        y * 3 + x
    else
        -1

gridCoords :: Point -> (Int, Int)
gridCoords (x, y) = (floor (x / gridSize + 1.5), floor (y / gridSize + 1.5))

drawWinsFor :: Player -> Board -> Picture
drawWinsFor p b = Pictures $ map drawWinSquare $ winsFor p b
    where drawWinSquare i = let (x, y) = markPositions!!i in Translate x y $ winSquare

markX, markO, highlightMarkX, highlightMarkO, backgroundGrid, horizontalLine, verticalLine,
    winSquare :: Picture

markX = Color red $ Pictures
    [ Rotate 45 $ rectangleSolid (gridSize * 0.75) lineWidth
    , Rotate (-45) $ rectangleSolid (gridSize * 0.75) lineWidth ]

markO = Color blue $ thickCircle (gridSize * 0.275) lineWidth

highlightMarkX = Color (makeColorI 255 220 220 255) $ Pictures
    [ Rotate 45 $ rectangleSolid (gridSize * 0.75) lineWidth
    , Rotate (-45) $ rectangleSolid (gridSize * 0.75) lineWidth ]

highlightMarkO = Color (makeColorI 220 220 255 255) $ thickCircle (gridSize * 0.275) lineWidth

backgroundGrid = Color (makeColorI 192 192 192 255) $ Pictures
    [ Translate (-r) 0  verticalLine
    , Translate r 0     verticalLine
    , Translate 0 (-r)  horizontalLine
    , Translate 0 r     horizontalLine
    ]
    where r = gridSize / 2

horizontalLine = rectangleSolid (gridSize * 3) lineWidth
verticalLine = rectangleSolid lineWidth (gridSize * 3)
winSquare = Color (makeColorI 200 255 200 255) $ rectangleSolid gridSize gridSize

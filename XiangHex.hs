import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Extent (Coord)
import Data.Maybe

data Side = Red | Blue
    deriving (Show, Eq, Enum, Bounded)
data PieceKind = Soldier | Horse | Chariot | Cannon | Elephant | Mandarin | General
    deriving (Show, Eq, Enum, Bounded)
type Piece = (Side, PieceKind, Coord)
type Board = [Piece]
data Game = Game {
    board :: Board,
    mousePos :: Point
    } deriving (Show)

hexRadius = 54 :: Float
hexWidth = (sqrt 3) / 2

main :: IO ()
main = play
        displayMode
        backgroundColor
        framesPerSecond
        newGame
        drawGame
        handleInputEvent
        stepTime
    where displayMode = InWindow "Xiang Hex" (sizeX, sizeY) (5, 5)
          backgroundColor = makeColor8 170 180 145 255
          framesPerSecond = 100
          sizeX = 1024
          sizeY = 768

stepTime :: Float -> Game -> Game
stepTime _ = id

drawGame :: Game -> Picture
drawGame g@(Game board mousePos) = Scale hexRadius hexRadius $ Pictures [
    drawBoard,
    drawPieces board,
    mouseHighlight g ]

mouseHighlight :: Game -> Picture
mouseHighlight (Game board mousePos) = case pieceAt board mouseGridPos of
    Just piece -> Pictures $ map highlightCoord $ legalNewCoordsForPiece board piece
    Nothing -> Blank
    where highlightCoord c = Color white $ translateCoord c $ hexHighlight
          mouseGridPos = gridFromScreen mousePos

handleInputEvent :: Event -> Game -> Game
handleInputEvent (EventKey (Char 'n') Down _ _) g = g { board = initialBoard }
-- handleInputEvent (EventKey (MouseButton LeftButton) Down _ pos) g = tryToMark (boardIndex pos) g
handleInputEvent (EventMotion pos) g = g { mousePos = pos }
handleInputEvent _ g = g

drawBoard :: Picture
drawBoard = Pictures $ [ Color (hexColor c) $ translateCoord c $ hex | c <- boardCoords ]

drawPieces :: Board -> Picture
drawPieces p = Pictures $ map drawPiece p

drawPiece :: Piece -> Picture
drawPiece (side, piece, pos) =
    Color (sideColor side) $ translateCoord pos $ pieceKindPicture piece

newGame :: Game
newGame = Game { board = initialBoard, mousePos = (1000, 0)}

pieceKindCoords :: PieceKind -> [Coord]
pieceKindCoords k = case k of
    General  -> [(5, 5)]
    Mandarin -> [(5, 4), (4, 5)]
    Elephant -> [(5, 3), (3, 5)]
    Horse    -> [(5, 2), (2, 5)]
    Chariot  -> [(5, 1), (1, 5)]
    Cannon   -> [(4, 2), (2, 4)] -- [(4, 1), (1, 4)]
    Soldier  -> [(4, 0), (3, 1), (2, 2), (1, 3), (0, 4)]

initialBoard :: Board
initialBoard = redPieces ++ bluePieces where
    redPieces  = [ (Red,  k, ( x,  y)) | (k, (x, y)) <- pieces ]
    bluePieces = [ (Blue, k, (-x, -y)) | (k, (x, y)) <- pieces ]
    pieces = [ (k, c) | k <- [minBound..], c <- pieceKindCoords k ]

boardCoords :: [Coord]
boardCoords = [ (i, j) | i <- [-5..5], j <- [-5..5], j - i < 5, i - j < 5 ]

isOnBoard :: Coord -> Bool
isOnBoard (i, j) = i >= -5 && j >= -5 && i <= 5 && j <= 5 && j - i < 5 && i - j < 5

isOffBoard = not . isOnBoard

isInPalace :: Side -> Coord -> Bool
isInPalace side (i, j) = case side of
  Red -> i >=  3 && j >=  3 && i - j < 2 && j - i < 2
  Blue -> i <= -3 && j <= -3 && i - j < 2 && j - i < 2

isInRiver :: Coord -> Bool
isInRiver (i, j) = i + j == 0

isInHomeField :: Side -> Coord -> Bool
isInHomeField side (i, j) = case side of
  Red -> i + j > 0
  Blue -> i + j < 0

hexColor :: Coord -> Color
hexColor c
    | isInPalace Blue c = lowerPalaceColor c
    | isInPalace Red c = upperPalaceColor c
    | isInRiver c = riverColor
    | otherwise = baseBoardColor c

baseBoardColor = cycleColors [ (85, 106, 47), (94, 117, 52), (77, 96, 43) ]
lowerPalaceColor = cycleColors [ (155, 114, 17), (211, 180, 8), (182, 140, 34) ]
upperPalaceColor = cycleColors [ (155, 114, 17), (182, 140, 34), (211, 180, 8) ]
riverColor = makeColor8 69 129 179 255

sideColor :: Side -> Color
sideColor Red = makeColor8 255 190 180 255
sideColor Blue = makeColor8 180 190 255 255

cycleColors :: [ (Int, Int, Int) ] -> Coord -> Color
cycleColors rgbs (i, j) = colors!!((i + j) `mod` (length colors))
    where colors = map (\(r, g, b) -> makeColor8 r g b 255) rgbs

translateCoord :: Coord -> Picture -> Picture
translateCoord (i, j) = Translate x y
    where x = fromIntegral (i + j) * hexWidth
          y = fromIntegral (i - j) * 1.5

gridFromScreen :: Point -> Coord
gridFromScreen (x, y) = (i, j)
    where xScaled = x / (hexRadius * hexWidth)
          yScaled = y / hexRadius
          yI = fromIntegral (floor (yScaled + xScaled / 2))
          yJ = fromIntegral (floor (yScaled - xScaled / 2))
          xBoth = fromIntegral (floor xScaled)
          i = floor ((xBoth + yI + 2) / 3)
          j = floor ((xBoth - yJ + 1) / 3)

hex :: Picture
hex = Polygon [ (0, -1), (x, -y), (x, y), (0, 1), (-x, y), (-x, -y) ]
    where x = hexWidth
          y = 0.5

hexHighlight :: Picture
hexHighlight = Circle 0.8

pieceKindPicture :: PieceKind -> Picture
pieceKindPicture kind = case kind of
    Soldier  -> circleSolid 0.5
    Horse    -> Polygon [ (0, -0.65), (0.5, 0.5), (-0.5, 0.5) ]
    Chariot  -> Pictures [ Translate 0 0.2 $ rectangleSolid 0.7 0.7, Translate 0 (-0.3) $ circleSolid 0.2 ]
    Cannon   -> Pictures [ Rotate (-15) $ Translate 0 0.2 $ rectangleSolid 1 0.3, circleSolid 0.3 ]
    Elephant -> Pictures [ Translate 0 0.2 $ circleSolid 0.45, rectangleSolid 0.4 1 ]
    Mandarin -> Rotate 45 $ rectangleSolid 0.8 0.8
    General  -> rectangleSolid 1 1

pieceAt :: Board -> Coord -> Maybe Piece
pieceAt board coord = listToMaybe $ filter (\(_, _, pos) -> pos == coord) board

isEmptyOrEnemy :: Board -> Side -> Coord -> Bool
isEmptyOrEnemy board side pos = isOnBoard pos && case pieceAt board pos of
    Nothing -> True
    Just (side', _, _) -> side /= side'

isEmpty :: Board -> Coord -> Bool
isEmpty board pos = isOnBoard pos && case pieceAt board pos of
    Nothing -> True
    Just _ -> False

{-

Maybe write some parser combinator type functions for describing moves?

pass i j: move through an unoccupied square
step i j: end move on an unoccupied square
capture i j: end move on a square occupied by an enemy piece, capturing it
jump i j: move through an occupied square
zeroOrMore: apply a combinator zero or more times

horse: pass orthogonal; step diagonal
chariot: zeroOrMore pass orthogonal; step orthogonal

-}

legalNewCoordsForPiece :: Board -> Piece -> [Coord]
legalNewCoordsForPiece board (side, kind, coord) = moves board side coord
    where moves = case kind of
            Soldier  -> soldierMoves
            Horse    -> horseMoves
            Chariot  -> chariotMoves
            Cannon   -> cannonMoves
            Elephant -> elephantMoves
            Mandarin -> mandarinMoves
            General  -> generalMoves

soldierMoves :: Board -> Side -> Coord -> [ Coord ]
soldierMoves board side pos = straightMove ++ diagonalMoves
    where
        straightMove = stepMoves board side pos [forwardDir side]
        diagonalMoves = filter (not . (isInHomeField side)) (stepMoves board side pos (sideDirs side))
        sideDirs Red = [(-1, 0), (0, -1), (-1, 1), (1, -1)]
        sideDirs Blue = [(1, 0), (0, 1), (-1, 1), (1, -1)]

horseMoves :: Board -> Side -> Coord -> [ Coord ]
horseMoves board side pos = concatMap f moves
  where f (ortho, diagonals) = let pos0 = pos |+| ortho in
          if isEmpty board pos0 then filter (isEmptyOrEnemy board side) (map (|+| pos0) diagonals)
          else []
        moves = [ ((1, 0), [(1, -1), (2, 1)]),
                  ((1, 1), [(1, 2), (2, 1)]),
                  ((0, 1), [(-1, 1), (1, 2)]),
                  ((-1, 0), [(-2, -1), (-1, 1)]),
                  ((-1, -1), [(-2, -1), (-1, -2)]),
                  ((0, -1), [(-1, -2), (1, -1)]) ]

chariotMoves :: Board -> Side -> Coord -> [ Coord ]
chariotMoves board side pos@(i0, j0) = concatMap movesForDir orthogonalDirs
    where
        movesForDir dir = slideMoves board side (pos |+| dir) dir
        slideMoves board side pos dir
          | isOffBoard pos = []
          | otherwise = case pieceAt board pos of
              Nothing -> pos : slideMoves board side (pos |+| dir) dir
              Just (side', _, _) -> if side' == side then [] else [ pos ]

cannonMoves :: Board -> Side -> Coord -> [ Coord ]
cannonMoves board side pos = concatMap movesForDir orthogonalDirs
    where
        movesForDir dir = slideMoves board side (pos |+| dir) dir
        slideMoves board side pos dir
          | isOffBoard pos = []
          | otherwise = case pieceAt board pos of
              Nothing -> pos : slideMoves board side (pos |+| dir) dir
              Just (_, _, _) -> let pos' = pos |+| dir in
                if (isOffBoard pos') then [] else
                  case pieceAt board pos' of
                    Nothing -> []
                    Just (side', _, _) -> if side' == side then [] else [pos']

elephantMoves :: Board -> Side -> Coord -> [ Coord ]
elephantMoves board side pos = concatMap movesForDir diagonalDirs
  where
    movesForDir dir = let pos0 = pos |+| dir in
      if (not (isEmpty board pos0)) then [] else
        let pos1 = pos0 |+| dir in
          if isEmptyOrEnemy board side pos1 then [pos1] else []

mandarinMoves :: Board -> Side -> Coord -> [Coord]
mandarinMoves board side coord = filter (isInPalace side) $ stepMoves board side coord diagonalDirs

-- Have to add the rule that a general cannot be on an empty file with the opposing general.
generalMoves :: Board -> Side -> Coord -> [Coord]
generalMoves board side coord = filter (isInPalace side) $ stepMoves board side coord orthogonalDirs

stepMoves :: Board -> Side -> Coord -> [Coord] -> [Coord]
stepMoves board side coord possibleMoves = filter (isEmptyOrEnemy board side) offsetPositions
    where offsetPositions = map (|+| coord) possibleMoves

orthogonalDirs :: [Coord]
orthogonalDirs = [ (1, 0), (-1, 0), (0, 1), (0, -1), (-1, -1), (1, 1) ]

diagonalDirs :: [Coord]
diagonalDirs = [ (2, 1), (1, -1), (1, 2), (-1, 1), (-2, -1), (-1, -2) ]

forwardDir :: Side -> Coord
forwardDir Red = (-1, -1)
forwardDir Blue = (1, 1)

(|+|) :: Coord -> Coord -> Coord
(i0, j0) |+| (i1, j1) = (i0 + i1, j0 + j1)

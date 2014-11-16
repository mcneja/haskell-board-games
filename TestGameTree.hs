import Data.Function (on)
import Data.List (intercalate, sortBy)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Maybe
import Data.Tree
import System.Random

-- Types

type Coord = (Int, Int)

data ETree n e = ENode {
    etRoot :: n,
    etChildren :: [(e, ETree n e)]
    }

data Side = Red | Blue deriving Eq

data PieceKind = Pawn | King deriving Eq

data Piece = Piece {
    pieceSide :: Side,
    pieceKind :: PieceKind
    }

type Pieces = Map Coord Piece

data Board = Board {
    bsPieces :: Pieces,
    bsSide :: Side
    }

data Action = Slide Coord | Capture [(Coord, Coord)]

type Move = (Coord, Action)

type GameTree = ETree Board Move

data Game = Game {
    gmTree :: GameTree,
    gmRandomSource :: StdGen
    }

type Score = Int

-- Values

boardSize :: Int
boardSize = 4

searchDepth :: Int
searchDepth = 3

gameBoard :: Game -> Board
gameBoard = etRoot . gmTree

gameNextTrees :: Game -> [GameTree]
gameNextTrees = (map snd) . etChildren . gmTree

main :: IO ()
main = do
    r <- getStdGen
    let g = newGame r
    printGame g
    runGame doComputerMove g
    -- runGame doRandomMove g

runGame :: (Game -> Game) -> Game -> IO ()
runGame f g = do
    let g' = f g
    printGame g'
    if null (etChildren (gmTree g')) then return () else runGame f g'

doRandomMove :: Game -> Game
doRandomMove g = case randomElement (gmRandomSource g) (gameNextTrees g) of
    Nothing -> g
    Just (tree, rnd) -> g { gmTree = tree, gmRandomSource = rnd }

doComputerMove :: Game -> Game
doComputerMove g = case randomElement (gmRandomSource g) bestMoves of
    Nothing -> g
    Just (tree, rnd) -> g { gmTree = tree, gmRandomSource = rnd }
    -- Just (tree, rnd) -> g { gmTree = gameTreeFrom (etRoot tree), gmRandomSource = rnd }
  where
    moves = gameNextTrees g
    scoredMoves = zip (map scoreTree moves) moves
    scoredMovesSorted = sortBy ((flip compare) `on` ((* scoreMult) . fst)) scoredMoves
    bestMoves = case scoredMovesSorted of
      [] -> []
      ((bestScore, _):_) -> map snd $ takeWhile ((== bestScore) . fst) scoredMovesSorted
    scoreMult = case (bsSide (gameBoard g)) of
      Red -> 1
      Blue -> -1

printGame :: Game -> IO ()
printGame g = do
    putStrLn ("B->" ++ (show (cBlue g)) ++ "       " ++ (show (cRed g)) ++ "<-R")
    putStr (drawBoard (etRoot (gmTree g)))
    where
      cRed = length . (filter ((== Red) . pieceSide)) . Map.elems . bsPieces . etRoot . gmTree
      cBlue = length . (filter ((== Blue) . pieceSide)) . Map.elems . bsPieces . etRoot . gmTree

drawBoard :: Board -> String
drawBoard b = concatMap drawRow [-boardSize..boardSize]
  where
    drawRow r = (replicate (abs r) ' ') ++ (intercalate " " (map drawSpot [0 .. (2*boardSize - (abs r))])) ++ "\n"
      where
        drawSpot c = case pieceAt pieces coord of
          Nothing -> "."
          Just (Piece side kind) -> case kind of
            Pawn -> if side == Blue then "b" else "r"
            King -> if side == Blue then "B" else "R"
          where
            coord = (c, c) |+| rowOrigin
        rowOrigin = (max (-boardSize) (r - boardSize), max (-boardSize) (-boardSize - r))
    pieces = bsPieces b

scoreTree :: GameTree -> Score
scoreTree = minimaxScoreTree searchDepth

minimaxScoreTree :: Int -> GameTree -> Score
minimaxScoreTree depth tree =
  if depth == 0 || null nextMoves
  then scoreBoard (etRoot tree)
  else minMax $ map (minimaxScoreTree (depth - 1)) nextMoves
  where
    nextMoves = map snd (etChildren tree)
    minMax = if (bsSide (etRoot tree)) == Red then maximum else minimum

scoreBoard :: Board -> Score
scoreBoard = sum . (map scorePiece) . Map.elems . bsPieces
  where
    scorePiece piece = case (pieceSide piece) of
      Red -> 1
      Blue -> -1

randomElement :: StdGen -> [a] -> Maybe (a, StdGen)
randomElement r xs = if (null xs) then Nothing else Just (xs!!i, r')
  where (i, r') = randomR (0, length xs - 1) r

newGame :: StdGen -> Game
newGame r = Game {
    gmTree = gameTreeFrom initialBoard,
    gmRandomSource = r
    }

initialBoard :: Board
initialBoard = Board {
    bsPieces = initialBoardPieces,
    bsSide = Blue
    }

initialBoardPieces :: Pieces
initialBoardPieces = Map.fromList (redPieces ++ bluePieces) where
    redPieces  = [ (( x,  y), Piece Red  Pawn) | (x, y) <- pieceCoords ]
    bluePieces = [ ((-x, -y), Piece Blue Pawn) | (x, y) <- pieceCoords ]
    pieceCoords = [ (x, y) | x <- [1..boardSize], y <- [1..boardSize] ]

boardCoords :: [Coord]
boardCoords = [ (i, j) |
    i <- [-boardSize..boardSize],
    j <- [-boardSize..boardSize],
    j - i <= boardSize,
    i - j <= boardSize ]

isOnBoard :: Coord -> Bool
isOnBoard (i, j) =
    i >= -boardSize &&
    j >= -boardSize &&
    i <= boardSize &&
    j <= boardSize &&
    j - i <= boardSize &&
    i - j <= boardSize

pieceAt :: Pieces -> Coord -> Maybe Piece
pieceAt board coord = Map.lookup coord board

isEnemy :: Board -> Coord -> Bool
isEnemy board coord = isOnBoard coord && case pieceAt (bsPieces board) coord of
    Nothing -> False
    Just piece -> (bsSide board) /= pieceSide piece

isEmpty :: Board -> Coord -> Bool
isEmpty board coord = isOnBoard coord && isNothing (pieceAt (bsPieces board) coord)

movesForPiece :: Board -> Coord -> Piece -> [Move]
movesForPiece board coord (Piece side kind) = map (\x -> (coord, x)) $
    if not (null captures)
      then map Capture (longest captures)
      else map Slide slides
    where
      board' = board { bsSide = side }
      captures = concatMap treePaths $ captureMoves capturePos board' coord
      slides = slidesFunc board' coord
      capturePos = case kind of
        Pawn -> pawnCapturePos
        King -> kingCapturePos
      slidesFunc = case kind of
        Pawn -> pawnSlides
        King -> kingSlides

gameTreeFrom :: Board -> GameTree
gameTreeFrom board = ENode board childStates
  where
    childStates = zip moves (map doMove moves)
    doMove = gameTreeFrom . (executeMove board)
    moves = movesForBoard board

executeMove :: Board -> Move -> Board
executeMove board (pos0, action) = case pieceAt (bsPieces board) pos0 of
    Nothing -> board
    Just p -> Board { bsPieces = bNew, bsSide = nextPlayer }
      where
        bNew = bsPieces board |> removePieceFromOldPos |> removeCapturedPieces |> insertPieceAtNewPos
        removePieceFromOldPos = Map.delete pos0
        removeCapturedPieces = case action of
          Slide _ -> id
          Capture c -> removeCaptures c
        insertPieceAtNewPos = Map.insert pos (promoteToKing pos p)
          where
            pos = finalDestination action
        nextPlayer = toggleSide (bsSide board)

promoteToKing :: Coord -> Piece -> Piece
promoteToKing coord piece = if (finalRow (pieceSide piece) coord)
    then piece { pieceKind = King }
    else piece

finalRow :: Side -> Coord -> Bool
finalRow side (i, j) = case side of
    Blue -> i >= boardSize || j >= boardSize
    Red -> i <= -boardSize || j <= -boardSize

removeCaptures :: [(Coord, Coord)] -> Pieces -> Pieces
removeCaptures captures board = foldl (flip Map.delete) board (map fst captures)

pawnSlides :: Board -> Coord -> [Coord]
pawnSlides board coord = stepMoves board coord (pawnDirs (bsSide board))

kingSlides :: Board -> Coord -> [Coord]
kingSlides board coord = concatMap (slideMoves board coord) allDirs

stepMoves :: Board -> Coord -> [Coord] -> [Coord]
stepMoves board coord possibleMoves = coordsNext
    where coordsNext = filter (isEmpty board) $ map (|+| coord) possibleMoves

slideMoves :: Board -> Coord -> Coord -> [Coord]
slideMoves board coord dir = let dest = coord |+| dir in
    if (isEmpty board dest) then (dest : (slideMoves board dest dir)) else []

captureMoves :: (Board -> Coord -> Coord -> Coord) -> Board -> Coord -> [Tree (Coord, Coord)]
captureMoves posFunc board = capturesFrom Set.empty
  where
    capturesFrom :: Set Coord -> Coord -> [Tree (Coord, Coord)]
    capturesFrom captured pos = mapMaybe (captures captured pos) allDirs

    captures :: Set Coord -> Coord -> Coord -> Maybe (Tree (Coord, Coord))
    captures captured pos0 dir = case validMove of
        True -> Just (Node (pos1, pos2) (capturesFrom (Set.insert pos1 captured) pos2))
        False -> Nothing
      where
        validMove =
          (isEnemy board pos1) &&
          (isEmpty board pos2) &&
          (Set.notMember pos1 captured)
        pos1 = posFunc board pos0 dir
        pos2 = pos1 |+| dir

pawnCapturePos :: Board -> Coord -> Coord -> Coord
pawnCapturePos _ pos0 dir = pos0 |+| dir

kingCapturePos :: Board -> Coord -> Coord -> Coord
kingCapturePos board pos0 dir = slide pos0
  where
    slide pos = let posNext = pos |+| dir in
      if (isEmpty board posNext) then slide posNext else posNext

treePaths :: Tree a -> [[a]]
treePaths (Node root nodes) = if null nodes then [[root]] else map (root:) (concatMap treePaths nodes)

longest :: [[a]] -> [[a]]
longest lists = filter ((== maxLength) . length) lists
    where
        maxLength = maximum (map length lists)

finalDestination :: Action -> Coord
finalDestination action = case action of
    Slide c -> c
    Capture cs -> snd (last cs)

legalMove :: Game -> Coord -> Coord -> Maybe GameTree
legalMove game pos0 pos1 = if null movesFiltered
    then Nothing
    else Just $ snd $ head movesFiltered
    where
      moves = etChildren (gmTree game)
      movesFrom = filter ((== pos0) . fst . fst) moves
      movesFiltered = filter ((== pos1) . finalDestination . snd . fst) movesFrom

movesForBoard :: Board -> [Move]
movesForBoard board = if not (null captureMoves) then longestCaptureMoves else unfilteredMoves
  where
    longestCaptureMoves = filter ((== longestMoveLength) . moveLength) captureMoves
    longestMoveLength = maximum (map moveLength captureMoves)
    captureMoves = filter moveIsCapture unfilteredMoves
    unfilteredMoves = concatMap (uncurry (movesForPiece board)) currentPieces
    currentPieces = filter ((== currentPlayer) . pieceSide . snd) $ Map.toList (bsPieces board)
    currentPlayer = bsSide board

moveIsCapture :: Move -> Bool
moveIsCapture (_, Slide _) = False
moveIsCapture (_, Capture _) = True

moveLength :: Move -> Int
moveLength (_, Slide _) = 0
moveLength (_, Capture c) = length c

pawnDirs :: Side -> [ Coord ]
pawnDirs side = case side of
    Red -> [(-1, -1), (-1, 0), (0, -1)]
    Blue -> [(1, 1), (1, 0), (0, 1)]

allDirs :: [ Coord ]
allDirs = [ (-1, -1), (-1, 0), (0, -1), (1, 0), (0, 1), (1, 1) ]

toggleSide :: Side -> Side
toggleSide Red = Blue
toggleSide Blue = Red

(|+|) :: Num a => (a, a) -> (a, a) -> (a, a)
(i0, j0) |+| (i1, j1) = (i0 + i1, j0 + j1)

(|>) :: a -> (a -> b) -> b
(|>) = flip ($)

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Extent (Coord)
import Data.List (foldl', nub, sortBy)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe
import System.Random
import System.Random.Shuffle

-- Types

data Terrain =
    Open |      -- Soldiers and aliens can be here
    Building |  -- Soldiers can be here
    Swamp |     -- Aliens can be here
    Rocket      -- Soldiers can be here, and if aliens touch it you lose
    deriving Eq

data PieceKind = Soldier | Alien deriving Eq

data Piece = Piece {
    pcCoord :: Coord,
    pcKind :: PieceKind
    }

type HexMap = Map Coord Terrain

type Tile = [Terrain]
type Rotation = Int
type TileMap = [(Coord, Rotation, Tile)]

data Board = Board {
    bsMap :: HexMap,
    bsPieces :: [Piece]
    }

data Game = Game {
    gmBoard :: Board,
    gmMousePos :: Point,
    gmRandomSource :: StdGen
    }

-- Values

boardSize :: Int
boardSize = 4

hexRadius :: Float
hexRadius = 16

hexWidth :: Float
hexWidth = (sqrt 3) / 2

main :: IO ()
main = do
    r <- getStdGen
    play
        displayMode
        backgroundColor
        framesPerSecond
        (newGame r)
        drawGame
        handleInputEvent
        stepTime
    where displayMode = InWindow "Crash" (sizeX, sizeY) (5, 5)
          backgroundColor = makeColor8 170 180 145 255
          framesPerSecond = 100
          sizeX = 768
          sizeY = 700

stepTime :: Float -> Game -> Game
stepTime _ = id

drawGame :: Game -> Picture
drawGame g = Scale hexRadius hexRadius $ Pictures [
    drawMap $ bsMap $ gmBoard g,
    drawPieces $ bsPieces $ gmBoard g
    ]

handleInputEvent :: Event -> Game -> Game
handleInputEvent e g = case e of
    (EventKey (Char 'n') Down _ pos) -> (newGame (gmRandomSource g)) { gmMousePos = pos }
    (EventKey (MouseButton LeftButton) Down _ pos) -> onMouseDown (onMouseMove pos g)
    (EventKey (MouseButton LeftButton) Up _ pos) -> onMouseUp (onMouseMove pos g)
    (EventKey (SpecialKey KeySpace) Down _ pos) -> moveMonsters (onMouseMove pos g)
    (EventMotion pos) -> onMouseMove pos g
    _ -> g

onMouseMove :: Point -> Game -> Game
onMouseMove p g = g { gmMousePos = p }

onMouseDown :: Game -> Game
onMouseDown g = g

onMouseUp :: Game -> Game
onMouseUp g = g

newGame :: StdGen -> Game
newGame r = Game {
    gmBoard = Board { bsMap = board, bsPieces = pieces },
    gmMousePos = (1000, 0),
    gmRandomSource = r'
    }
    where (r', board, pieces) = randomTileMap r

randomTileMap :: StdGen -> (StdGen, HexMap, [Piece])
randomTileMap rng0 = (rng2, board, pieces)
    where
      (board, pieces) = buildMap tilemap
      (tileListShuffled, rng1) = shuffleList tileList rng0
      (rng2, tilemap) = foldl' placeTileRandomly (rng1, initialTileMap) tileListShuffled
      tileList = (take 30 (repeat openTile)) ++ (take 10 (repeat swampTile)) ++ (take 5 (repeat buildingTile))
      -- tileList = (take 25 (repeat openTile)) ++ (take 10 (repeat swampTile)) ++ (take 10 (repeat buildingTile))
      -- tileList = (take 16 (repeat openTile)) ++ (take 7 (repeat swampTile)) ++ (take 7 (repeat buildingTile))

placeTileRandomly :: (StdGen, TileMap) -> Tile -> (StdGen, TileMap)
placeTileRandomly (rng, tilemap) tile = (rng'', (coord, rot, tile) : tilemap)
  where
    tileCoords = map (\(coord, _, _) -> coord) tilemap
    allCoords = nub $ [ i |+| j | i <- tileCoords, j <- neighborCoords ]
    adjCoords = filter (`notElem` tileCoords) allCoords
    validCoords = filter (\c -> hasAtLeastTwoNeighbors c) adjCoords
    hasAtLeastTwoNeighbors c = (>= 2) $ length $ filter (\c2 -> elem (c |+| c2) tileCoords) neighborCoords
    (i, rng') = randomR (0, length validCoords - 1) rng
    coord = validCoords !! i
    (rot, rng'') = randomR (0, 5) rng'

moveMonsters :: Game -> Game
moveMonsters g = g { gmBoard = (gmBoard g) { bsPieces = pieces }, gmRandomSource = rng' }
  where
    piecesOrig = bsPieces (gmBoard g)
    pieces = map (\p -> p { pcCoord = slideDest (pcCoord p) }) piecesOrig
    (i, rng') = randomR (0, 5) (gmRandomSource g)
    dir = neighborCoords!!i
    slideDest :: Coord -> Coord
    slideDest coord = if isRocket coord then coord
        else if isValid (coord |+| dir) then slideDest (coord |+| dir) else coord
    isValid :: Coord -> Bool
    isValid coord = case Map.lookup coord (bsMap (gmBoard g)) of
      Nothing -> False
      Just terrain -> terrain /= Building
    isRocket coord = case Map.lookup coord (bsMap (gmBoard g)) of
      Nothing -> False
      Just terrain -> terrain == Rocket

-- |Given a sequence (e1,...en) to shuffle, its length, and a random
-- generator, compute the corresponding permutation of the input
-- sequence.
shuffleList :: RandomGen gen => [a] -> gen -> ([a], gen)
shuffleList elements gen = (shuffle elements indexes, gen')
  where
    (indexes, gen') = rseq ((length elements) - 1) gen
    -- The sequence (r1,...r[n-1]) of numbers such that r[i] is an
    -- independent sample from a uniform random distribution
    -- [0..n-i]
    rseq :: RandomGen gen => Int -> gen -> ([Int], gen)
    rseq 0 gen = ([], gen)
    rseq i gen = (j : js, gen'')
      where
        (j, gen') = randomR (0, i) gen
        (js, gen'') = rseq (i - 1) gen'

neighborCoords :: [Coord]
neighborCoords = [ (-1, -1), (-1, 0), (0, -1), (1, 0), (0, 1), (1, 1) ]

drawMap :: HexMap -> Picture
drawMap m = Pictures $ map (uncurry drawTile) (Map.toList m)

drawPieces :: [Piece] -> Picture
drawPieces = Pictures . (map drawPiece)

drawPiece :: Piece -> Picture
drawPiece piece = Color color $ translateCoord (pcCoord piece) $ circleSolid 0.7
  where
    color = case (pcKind piece) of
          Alien -> red
          Soldier -> blue

drawTile :: Coord -> Terrain -> Picture
drawTile coord terrain = Color (hexColor coord terrain) $ translateCoord coord hex

buildMap :: TileMap -> (HexMap, [Piece])
buildMap tiles = (board, pieces)
  where
    (board, pieces) = foldl' (\m (c, r, t) -> addTile c r t m) (Map.empty, []) tiles

initialTileMap :: TileMap
initialTileMap = [
    ((0, 0), 2, rocketTile),
    ((1, 0), 4, rocketTile),
    ((1, 1), 0, rocketTile)
    ]

allSame :: Terrain -> Tile
allSame t = take 7 (repeat t)

openTile :: [Terrain]
openTile = [Swamp, Open, Open, Open, Open, Open, Open] -- allSame Open

swampTile :: [Terrain]
swampTile = [Open, Swamp, Swamp, Swamp, Swamp, Swamp, Open] -- allSame Swamp

buildingTile :: [Terrain]
buildingTile = [Building, Open, Open, Building, Open, Open, Open]

rocketTile :: [Terrain]
rocketTile = [Rocket, Open, Open, Rocket, Open, Open, Open]

addTile :: Coord -> Rotation -> Tile -> (HexMap, [Piece]) -> (HexMap, [Piece])
addTile clusterCoord rot terrains (m, p) = (m', p')
  where
    m' = foldl' addSubTile m (zip (map transform coords) terrains)
    p' = if isTerrainOpen then Piece { pcCoord = transform (0, 0), pcKind = Alien } : p else p
    addSubTile m (coord, terrain) = Map.insert coord terrain m
    transform = transformCoord clusterCoord rot
    coords = [(-1, -1), (0, -1), (-1, 0), (0, 0), (1, 0), (0, 1), (1, 1)]
    isTerrainOpen = case terrains!!3 of
      Open -> True
      Swamp -> True
      otherwise -> False

transformCoord :: Coord -> Rotation -> Coord -> Coord
transformCoord (clusterI, clusterJ) rot (i, j) = (originI + dirII * i + dirJI * j, originJ + dirIJ * i + dirJJ * j)
  where
    originI = 2 * clusterI + clusterJ
    originJ = 3 * clusterJ - clusterI
    ((dirII, dirIJ), (dirJI, dirJJ)) = rotationAxes !! rot

rotationAxes :: [(Coord, Coord)]
rotationAxes = [
    ((1, 0), (0, 1)),
    ((1, 1), (-1, 0)),
    ((0, 1), (-1, -1)),
    ((-1, 0), (0, -1)),
    ((-1, -1), (1, 0)),
    ((0, -1), (1, 1))
    ]

hexCoordFromClusterCoord :: Coord -> Coord
hexCoordFromClusterCoord (i, j) = (2 * i + j, 3 * j - i)

openHexColors :: [ Color ]
openHexColors = map (\(r, g, b) -> makeColor8 r g b 255) colors
    where colors = [ (85, 106, 47), (94, 117, 52), (77, 96, 43) ]

buildingHexColors :: [ Color ]
buildingHexColors = map (\c -> mixColors 0.5 0.5 black c) openHexColors

swampHexColors :: [ Color ]
swampHexColors = map (\c -> mixColors 0.25 0.55 blue c) openHexColors

rocketHexColors :: [ Color ]
rocketHexColors = map (\c -> mixColors 0.45 0.5 yellow c) openHexColors

hexColor :: Coord -> Terrain -> Color
hexColor coord terrain = cycleColors colors coord
    where colors = case terrain of
                    Open -> openHexColors
                    Building -> buildingHexColors
                    Swamp -> swampHexColors
                    Rocket -> rocketHexColors

cycleColors :: [ Color ] -> Coord -> Color
cycleColors colors (i, j) = colors!!((i + j) `mod` (length colors))

translateCoord :: Coord -> Picture -> Picture
translateCoord = (uncurry Translate) . screenFromGrid

screenFromGrid :: Coord -> Point
screenFromGrid (i, j) = (x, y)
    where x = fromIntegral (i + j) * hexWidth
          y = fromIntegral (i - j) * 1.5

gridFromScreen :: Point -> Coord
gridFromScreen (xScreen, yScreen) = (i, j)
    where xScaled = xScreen / (hexRadius * hexWidth)
          yScaled = yScreen / hexRadius
          yI = floor (yScaled + xScaled / 2)
          yJ = floor (yScaled - xScaled / 2)
          xIJ = floor xScaled
          i = floor ((fromIntegral (xIJ + yI + 2 :: Int)) / 3 :: Float)
          j = floor ((fromIntegral (xIJ - yJ + 1 :: Int)) / 3 :: Float)

hex :: Picture
hex = Polygon [ (0, -1), (x, -y), (x, y), (0, 1), (-x, y), (-x, -y) ]
    where x = hexWidth
          y = 0.5

(|+|) :: Num a => (a, a) -> (a, a) -> (a, a)
(i0, j0) |+| (i1, j1) = (i0 + i1, j0 + j1)

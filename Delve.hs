import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.Function (on)
import Data.List (nub, sortBy, (\\))
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Maybe
import Data.Tree
import System.Random

-- Types

type Coord = (Int, Int)

data Tile = Start | Straight | Ell | Tee | Cross | Cap deriving (Eq, Enum, Bounded)

data Orientation = East | North | West | South deriving (Eq, Enum, Bounded)

type Tilemap = Map Coord (Orientation, Tile)

data Game = Game {
    gmTilemap :: Tilemap,
    gmMousePos :: Point,
    gmRandomSource :: StdGen
    }

-- Values

tileSize :: Float
tileSize = 32

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
    where displayMode = InWindow "Delve" (sizeX, sizeY) (5, 5)
          backgroundColor = makeColorI 170 180 145 255
          framesPerSecond = 100
          sizeX = 768
          sizeY = 700

stepTime :: Float -> Game -> Game
stepTime _ = id

drawGame :: Game -> Picture
drawGame g = Scale tileSize tileSize $ Pictures [ drawTilemap g, drawAdjacencies g ]

handleInputEvent :: Event -> Game -> Game
handleInputEvent e g = case e of
    (EventKey (Char 'n') Down _ pos) -> (newGame (gmRandomSource g)) { gmMousePos = pos }
    (EventMotion pos) -> onMouseMove pos g
    (EventKey (SpecialKey KeySpace) Down _ pos) -> placeRandomTile (onMouseMove pos g)
    _ -> g

onMouseMove :: Point -> Game -> Game
onMouseMove p g = g { gmMousePos = p }

placeRandomTile :: Game -> Game
placeRandomTile g = g { gmTilemap = tilemap', gmRandomSource = r' }
    where
    (tilemap', r') = tryAddTile (gmRandomSource g) (gmTilemap g)

drawTilemap :: Game -> Picture
drawTilemap g = Pictures $ map drawTile $ Map.toList $ gmTilemap g
  where
  drawTile :: (Coord, (Orientation, Tile)) -> Picture
  drawTile (pos, (orient, tile)) = positionTile pos orient (tilePic tile)

drawAdjacencies :: Game -> Picture
drawAdjacencies g = Pictures $ map adjPic $ adjacentCoords $ gmTilemap g
    where
    adjPic :: Coord -> Picture
    adjPic pos = positionTile pos East $ spawnPic

positionTile :: Coord -> Orientation -> Picture -> Picture
positionTile (x, y) orient pic = Translate (fromIntegral x) (fromIntegral y) $ Rotate (rotationAngle orient) $ Translate (-0.5) (-0.5) pic

tilePic :: Tile -> Picture
tilePic t = Pictures [base, floor]
    where
    base = Color (greyN 0.5) $ Polygon [ (0, 0), (1, 0), (1, 1), (0, 1) ]
    floor = Color black $ Pictures $ case t of
        Start -> [ Polygon [ (0.2, 0.3), (0.3, 0.2), (1, 0.2), (1, 0.8), (0.3, 0.8), (0.2, 0.7) ] ]
        Straight -> [ Polygon [ (0, 0.2), (1, 0.2), (1, 0.8), (0, 0.8) ] ]
        Ell -> [ Polygon [ (0, 0.2), (0.8, 0.2), (0.2, 0.8), (0, 0.8) ],
                 Polygon [ (0.2, 0.8), (0.8, 0.2), (0.8, 1), (0.2, 1) ] ]
        Tee -> [ Polygon [ (0, 0.2), (0.2, 0.2), (0.2, 0.8), (0, 0.8) ],
                 Polygon [ (0.2, 0), (0.8, 0), (0.8, 1), (0.2, 1), (0.8, 0.2), (0.2, 0.2) ] ]
        Cross -> [
                 Polygon [ (0, 0.2), (1, 0.2), (1, 0.8), (0, 0.8) ],
                 Polygon [ (0.2, 0), (0.8, 0), (0.8, 1), (0.2, 1) ] ]
        Cap -> [ Polygon [ (0, 0.2), (0.8, 0.2), (0.8, 0.8), (0, 0.8) ] ]

spawnPic :: Picture
spawnPic = Color (makeColorI 255 0 0 64) $ Translate 0.5 0.5 $ circleSolid 0.45

newGame :: StdGen -> Game
newGame r = Game {
    gmTilemap = initialTilemap,
    gmMousePos = (1000, 0),
    gmRandomSource = r
    }

initialTilemap :: Tilemap
initialTilemap = Map.fromList [ ((0, 0), (East, Start)) ]

tileNeighborCoords :: Tile -> [ Coord ]
tileNeighborCoords t = case t of
    Start -> [ (1, 0) ]
    Straight -> [ (-1, 0), (1, 0) ]
    Ell -> [ (-1, 0), (0, 1) ]
    Tee -> [ (-1, 0), (0, -1), (0, 1) ]
    Cross -> [ (-1, 0), (1, 0), (0, -1), (0, 1) ]
    Cap -> [ (-1, 0) ]

rotateDir :: Orientation -> Coord -> Coord
rotateDir orient (x, y) = case orient of
    East -> (x, y)
    North -> (-y, x)
    West -> (-x, -y)
    South -> (y, -x)

unrotateDir :: Orientation -> Coord -> Coord
unrotateDir orient (x, y) = case orient of
    East -> (x, y)
    North -> (y, -x)
    West -> (-x, -y)
    South -> (-y, x)

rotationAngle :: Orientation -> Float
rotationAngle orient = case orient of
    East -> 0
    North -> 270
    West -> 180
    South -> 90

adjacentCoords :: Tilemap -> [ Coord ]
adjacentCoords tilemap = (nub $ concatMap tileAdjacentCoords (Map.toList tilemap)) \\ tileCoords
    where
    tileAdjacentCoords :: (Coord, (Orientation, Tile)) -> [Coord]
    tileAdjacentCoords (pos, (orient, tile)) = map ((|+| pos) . (rotateDir orient)) (tileNeighborCoords tile)

    tileCoords = Map.keys tilemap

isValidPlacement :: Tilemap -> Tile -> (Coord, Orientation) -> Bool
isValidPlacement tilemap tile (pos, orient) = and $ map valid adjacentDirs
    where
    valid :: Coord -> Bool
    valid dir = case Map.lookup (pos |+| dir) tilemap of
        Just (orient1, tile1) -> tilesMatch (orient, tile) (orient1, tile1) dir
        Nothing -> True

tilesMatch :: (Orientation, Tile) -> (Orientation, Tile) -> Coord -> Bool
tilesMatch (orient0, tile0) (orient1, tile1) (x, y) = (door0 == door1)
    where
    door0 = elem (unrotateDir orient0 ( x,  y)) (tileNeighborCoords tile0)
    door1 = elem (unrotateDir orient1 (-x, -y)) (tileNeighborCoords tile1)

tryAddTile :: StdGen -> Tilemap -> (Tilemap, StdGen)
tryAddTile r tilemap = case randomElement r' placements of
    Just (newTilemap, r'') -> (newTilemap, r'')
    Nothing -> (tilemap, r')

    where

    Just (tile, r') = randomElement r [Straight .. maxBound]
    allPlacements = possibleTilePlacements tilemap tile

    allPlacementsWithScore = map (placementWithScore tile) allPlacements

    placementsAvoidingClosure = filter (\(score, _) -> score > 0) allPlacementsWithScore
    -- placementsAvoidingClosure = allPlacementsWithScore

    minimumScore = if null placementsAvoidingClosure then 0 else minimum (map fst placementsAvoidingClosure)

    minimizingPlacements = filter (\(score, _) -> score == minimumScore) placementsAvoidingClosure

    placements = map snd minimizingPlacements

    placementWithScore :: Tile -> (Coord, Orientation) -> (Int, Tilemap)
    placementWithScore tile placement = (length (adjacentCoords newTilemap), newTilemap)
        where
        newTilemap = addTile tile placement

    addTile :: Tile -> (Coord, Orientation) -> Tilemap
    addTile tile (pos, orient) = Map.insert pos (orient, tile) tilemap

possibleTilePlacements :: Tilemap -> Tile -> [ (Coord, Orientation) ]
possibleTilePlacements tilemap tile = filter (isValidPlacement tilemap tile) allPlacements
    where
    allPlacements = [ (pos, orient) | pos <- positions, orient <- orientations ]
    positions = adjacentCoords tilemap
    orientations = [minBound .. maxBound]

randomElement :: StdGen -> [a] -> Maybe (a, StdGen)
randomElement r xs = if (null xs) then Nothing else Just (xs!!i, r')
  where (i, r') = randomR (0, length xs - 1) r

translateCoord :: Coord -> Picture -> Picture
translateCoord = (uncurry Translate) . screenFromGrid

adjacentDirs :: [Coord]
adjacentDirs = [ (-1, 0), (1, 0), (0, -1), (0, 1) ]

screenFromGrid :: Coord -> Point
screenFromGrid (i, j) = (x, y)
    where
    x = (fromIntegral i) * tileSize
    y = (fromIntegral j) * tileSize

gridFromScreen :: Point -> Coord
gridFromScreen (xScreen, yScreen) = (i, j)
    where
    i = floor (xScreen / tileSize)
    j = floor (yScreen / tileSize)

(|+|) :: Num a => (a, a) -> (a, a) -> (a, a)
(i0, j0) |+| (i1, j1) = (i0 + i1, j0 + j1)

(|*|) :: Num a => (a, a) -> a -> (a, a)
(x, y) |*| s = (x * s, y * s)

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.Function (on)
import Data.List (nub, sortBy, (\\))
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Set.Extra as Set
import Data.Maybe
import Data.Tree
import System.Random

-- Types

type Coord = (Int, Int)

data Tile = White | Black deriving Eq

type Tilemap = Map Coord Tile

data Game = Game {
    gmTilemap :: Tilemap,
    gmMousePos :: Point,
    gmRandomSource :: StdGen
    }

-- Values

hexWidth = (sqrt 3) / 2

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
    where
    displayMode = InWindow "Xia" (sizeX, sizeY) (5, 5)
    backgroundColor = makeColorI 64 96 128 255
    framesPerSecond = 100
    sizeX = 768
    sizeY = 700

stepTime :: Float -> Game -> Game
stepTime _ = id

drawGame :: Game -> Picture
drawGame g = Scale tileSize tileSize $ Pictures [
    highlightCoords (availableCoords g),
    drawMouseHighlight g,
    drawTilemap g ]

handleInputEvent :: Event -> Game -> Game
handleInputEvent e g = case e of
    (EventKey (Char 'n') Down _ pos) -> (newGame (gmRandomSource g)) { gmMousePos = pos }
    (EventKey (MouseButton LeftButton) Down _ pos) -> onMouseDown (onMouseMove pos g)
    (EventMotion pos) -> onMouseMove pos g
    (EventKey (SpecialKey KeySpace) Down _ pos) -> warStep (onMouseMove pos g)
    _ -> g

onMouseMove :: Point -> Game -> Game
onMouseMove p g = g { gmMousePos = p }

onMouseDown :: Game -> Game
onMouseDown g = case validPlacementCoord mouseGridPos of
    False -> g
    True -> g { gmTilemap = Map.insert mouseGridPos nextTileColor (gmTilemap g) }

    where

    mouseScreenPos = gmMousePos g
    mouseGridPos = gridFromScreen mouseScreenPos

    validPlacementCoord :: Coord -> Bool
    validPlacementCoord c = Set.member c (availableCoords g)

    (whites, blacks) = Map.partition (== White) (gmTilemap g)

    nextTileColor = if (Map.size whites) > (Map.size blacks) then Black else White

warStep :: Game -> Game
warStep g = g { gmTilemap = newTilemap, gmRandomSource = newRandom }
    where
    (newTilemap, newRandom) = tryCaptureTile ((gmTilemap g), (gmRandomSource g))

tryCaptureTile :: (Tilemap, StdGen) -> (Tilemap, StdGen)
tryCaptureTile (tilemap, r) = case randomElement (Map.toList tilemap) r of
    Nothing -> (tilemap, r)
    Just ((attackerCoord, attackerSide), r2) -> tryCaptureFrom (tilemap, r2)

        where

        tryCaptureFrom :: (Tilemap, StdGen) -> (Tilemap, StdGen)
        tryCaptureFrom (tilemap, r) = case randomElement (Set.toList enemyCoords) r of
            Just (attackedCoord, r2) -> (Map.insert attackedCoord attackerSide tilemap, r2)
            Nothing -> (tilemap, r)

        territoryCoords = contiguousTerritory tilemap attackerCoord attackerSide

        enemyCoords = adjacentTerritory tilemap territoryCoords

contiguousTerritory :: Tilemap -> Coord -> Tile -> Set Coord
contiguousTerritory tilemap coord side = floodFill setInit acceptMember adjacentMembers
    where

    setInit :: Set Coord
    setInit = Set.singleton coord
    
    acceptMember :: Coord -> Bool
    acceptMember c = case Map.lookup c tilemap of
        Nothing -> False
        Just side' -> side' == side

    adjacentMembers :: Coord -> Set Coord
    adjacentMembers c = Set.intersection (Map.keysSet tilemap) (adjacentCoords c)

adjacentTerritory :: Tilemap -> Set Coord -> Set Coord
adjacentTerritory tilemap coords = Set.intersection existingCoords adjCoords
    where
    existingCoords = Map.keysSet tilemap
    adjCoords = Set.difference (Set.concatMap adjacentCoords coords) coords

drawTilemap :: Game -> Picture
drawTilemap g = Pictures $ map drawTile $ Map.toList $ gmTilemap g
    where
    drawTile :: (Coord, Tile) -> Picture
    drawTile (pos, tile) = translateCoord pos (tilePic tile)

highlightCoords :: Set Coord -> Picture
highlightCoords coords = Color (greyN 0.25) $ Pictures $ map outlineCoord $ Set.toList coords
    where
    outlineCoord :: Coord -> Picture
    outlineCoord coord = translateCoord coord $ Scale 0.9 0.9 $ hexOutline

availableCoords :: Game -> Set Coord
availableCoords g = coords
    where
    coords = Set.filter (\c -> (numAdjacent usedCoords c) > 1) freeCoords
    freeCoords = Set.difference adjCoords usedCoords
    adjCoords = Set.concatMap adjacentCoords usedCoords
    usedCoords = Map.keysSet (gmTilemap g)

adjacentCoords :: Coord -> Set Coord
adjacentCoords (x, y) = Set.fromList [ (x + 1, y), (x, y + 1), (x - 1, y), (x, y - 1), (x + 1, y + 1), (x - 1, y - 1) ]

floodFill :: Ord a => Set a -> (a -> Bool) -> (a -> Set a) -> Set a
floodFill setInit acceptMember adjacentMembers = setFinal
    where
    setNeighbors = Set.concatMap (Set.filter acceptMember . adjacentMembers) setInit
    setNext = Set.union setInit setNeighbors
    setFinal = if setNext == setInit
        then setInit
        else floodFill setNext acceptMember adjacentMembers

numAdjacent :: Set Coord -> Coord -> Int
numAdjacent coords coord = Set.size $ Set.intersection coords $ adjacentCoords coord

drawMouseHighlight :: Game -> Picture
drawMouseHighlight g = translateCoord (gridFromScreen (gmMousePos g)) $ Color white $ hexOutline

tilePic :: Tile -> Picture
tilePic t = Color (makeColorI r g b 255) $ Scale 0.9 0.9 $ hex
    where
    (r, g, b) = rgbForTile t

rgbForTile :: Tile -> (Int, Int, Int)
rgbForTile t = case t of
    White -> (192, 192, 192)
    Black -> (32, 32, 32)

hex :: Picture
hex = Polygon [ (0, -1), (x, -y), (x, y), (0, 1), (-x, y), (-x, -y) ]
    where
    x = hexWidth
    y = 0.5

hexOutline :: Picture
hexOutline = lineLoop [ (0, -1), (x, -y), (x, y), (0, 1), (-x, y), (-x, -y) ]
    where
    x = hexWidth
    y = 0.5
    
newGame :: StdGen -> Game
newGame r = Game {
    gmTilemap = initialTilemap,
    gmMousePos = (1000, 0),
    gmRandomSource = r
    }

initialTilemap :: Tilemap
initialTilemap = Map.fromList [ ((0, 0), White), ((1, 1), Black) ]

randomElement :: [a] -> StdGen -> Maybe (a, StdGen)
randomElement xs r = if (null xs) then Nothing else Just (xs!!i, r')
    where (i, r') = randomR (0, length xs - 1) r

translateCoord :: Coord -> Picture -> Picture
translateCoord = (uncurry Translate) . screenFromGrid

screenFromGrid :: Coord -> Point
screenFromGrid (i, j) = (x, y)
    where
    x = fromIntegral (i + j) * hexWidth
    y = fromIntegral (i - j) * 1.5

gridFromScreen :: Point -> Coord
gridFromScreen (x, y) = (i, j)
    where
    xScaled = x / (tileSize * hexWidth)
    yScaled = y / tileSize
    yI = fromIntegral (floor (yScaled + xScaled / 2))
    yJ = fromIntegral (floor (yScaled - xScaled / 2))
    xBoth = fromIntegral (floor xScaled)
    i = floor ((xBoth + yI + 2) / 3)
    j = floor ((xBoth - yJ + 1) / 3)

(|+|) :: Num a => (a, a) -> (a, a) -> (a, a)
(i0, j0) |+| (i1, j1) = (i0 + i1, j0 + j1)

(|*|) :: Num a => (a, a) -> a -> (a, a)
(x, y) |*| s = (x * s, y * s)

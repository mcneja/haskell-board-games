import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Extent (Coord)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Maybe
import System.Random

-- Types

data Star = Star {
    starPos :: Point
    }

data Game = Game {
    gmStars :: [Star],
    gmMousePos :: Point,
    gmDragOrigin :: Maybe Star,
    gmRandomSource :: StdGen
    }

-- Values

starRadius :: Float
starRadius = 16

minStarSeparation :: Float
minStarSeparation = starRadius * 2.5

boardSizeX :: Int
boardSizeX = 800

boardSizeY :: Int
boardSizeY = 800

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
    where displayMode = InWindow "Galaxy" (boardSizeX, boardSizeY) (5, 5)
          backgroundColor = makeColor8 16 16 48 255
          framesPerSecond = 100

stepTime :: Float -> Game -> Game
stepTime _ = id

drawGame :: Game -> Picture
drawGame g = drawStars g

handleInputEvent :: Event -> Game -> Game
handleInputEvent e g = case e of
    (EventKey (Char 'n') Down _ pos) -> (newGame (gmRandomSource g)) { gmMousePos = pos }
    (EventMotion pos) -> onMouseMove pos g
    _ -> g

onMouseMove :: Point -> Game -> Game
onMouseMove p g = g { gmMousePos = p }

drawStars :: Game -> Picture
drawStars g = Pictures $ map drawStar (gmStars g)

drawStar :: Star -> Picture
drawStar star = Translate x y $ Color (makeColor8 190 190 224 255) $ circleSolid starRadius
  where
    (x, y) = starPos star

newGame :: StdGen -> Game
newGame r = Game {
    gmStars = stars,
    gmMousePos = (1000, 0),
    gmDragOrigin = Nothing,
    gmRandomSource = r'
    }
    where
      stars = map Star positions
      (positions, r') = randomStarPositions r

randomStarPositions :: StdGen -> ([Point], StdGen)
randomStarPositions r0 = iterate step ([], r0) !! 1000
  where
    step :: ([Point], StdGen) -> ([Point], StdGen)
    step (ps, r) = (ps', r')
      where
        (p, r') = randomStarPosition r
        ps' = if positionIsUsable ps p then p:ps else ps

positionIsUsable :: [Point] -> Point -> Bool
positionIsUsable setPos pos = not $ any (tooClose pos) setPos

tooClose :: Point -> Point -> Bool
tooClose (x0, y0) (x1, y1) = dx * dx + dy * dy < minStarSeparation * minStarSeparation
  where
    dx = x1 - x0
    dy = y1 - y0

randomStarPosition :: StdGen -> (Point, StdGen)
randomStarPosition r0 = ((x, y), r2)
  where
    (x, r1) = randomR (-rx, rx) r0
    (y, r2) = randomR (-ry, ry) r1
    rx = (fromIntegral boardSizeX) / 2 - starRadius * 1.5
    ry = (fromIntegral boardSizeY) / 2 - starRadius * 1.5

data Node a = Node {
    label :: a,
    adjacent :: [Node a]
    }

newtype Graph a = Graph [Node a]

mkGraph :: Eq a => [(a, [a])] -> Graph a
mkGraph links = Graph $ map snd nodeLookupList
  where
    nodeLookupList = map mkNode links
    mkNode (lbl, adj) = (lbl, Node lbl $ map lookupNode adj)
    lookupNode lbl = fromJust $ lookup lbl nodeLookupList

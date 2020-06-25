import Control.Monad.Random
import Control.Monad

type RandGen = Rand StdGen
type RandGenIO = RandT StdGen IO

main = do
    g <- getStdGen
    let (r, g') = runRand twoEliteNumbers g
    putStrLn $ "Result is: " ++ show r

twoEliteNumbers :: RandGen (Double, Double)
twoEliteNumbers = do
    number1 <- eliteNumber
    number2 <- eliteNumber
    return (number1, number2)

eliteNumber :: RandGen Double
eliteNumber = do
    randomNumber <- getRandomR (0.0, 1.0)
    return (randomNumber * 1337)


oldMain = do
    g <- getStdGen
    evalRand rolls g
    evalRand newRoll g
    let rs = evalRand (getRandomList 12) g
    putStr $ "Randoms:\n" ++ concatMap (\s -> "    " ++ show s ++ "\n") rs

roll :: StdGen -> IO ()
roll g = do
    let r = evalRand (getRandomR (0 :: Double, 1)) g -- evalRand getRandomNormal g
    putStrLn $ "Result: " ++ show r

rollD6 :: RandGenIO Int
rollD6 = do
    r <- getRandomR (1, 6)
    return r

rolls :: RandGen (IO ())
rolls = do
    r0 <- getRandomR (0 :: Double, 1)
    r1 <- getRandomR (0 :: Double, 1)
    return (putStrLn ("Two random numbers: " ++ show r0 ++ ", " ++ show r1))

newRoll :: RandGen (IO ())
newRoll = do
    r <- getRandomR (0 :: Double, 1) -- getRandomNormal
    return (putStrLn $ "Result: " ++ show r)

getRandomList :: Int -> RandGen [Double]
getRandomList n = do
    rs <- replicateM n (getRandomR (0, 1))
    return rs

getRandomNormal :: RandGen Double
getRandomNormal = do
    let n = 12
    rs <- getRandomList n
    return ((sum rs) / (fromIntegral n))

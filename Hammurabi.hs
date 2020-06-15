-- Converted from the original FOCAL program and modified for Edusystem 70 by David Ahl, Digital
-- Modified for 8K Microsoft BASIC by Peter Turnbull
-- Ported to Haskell by James McNeill

{-
TODO Add a help command
     Help would give you some of the things to think about, and maybe some specific numbers.
-}

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Random
import Data.List (minimumBy)
import Data.Ord (comparing)
import System.Exit (exitSuccess)
import System.Random
import System.IO (hFlush, stdout)
import Text.Printf (printf)
import Text.Read (readMaybe)

-- Data structures

data GameState = GameState {
    year         :: Int,
    people       :: Int,
    food         :: Int, -- bushels
    land         :: Int, -- acres
    totalDeaths  :: Int,
    cumDeathRate :: Double
} deriving (Show)

data Results = Results {
    peopleStarved      :: Int,
    peopleDiedOfPlague :: Int,
    peopleBorn         :: Int,
    bushelsEatenByRats :: Int,
    bushelsPerAcre     :: Int
} deriving (Show)

type RandIO = RandT StdGen IO

-- Code

initialState = GameState {
    year = 0,
    people = 100,
    food = 2800,
    land = 1000,
    totalDeaths = 0,
    cumDeathRate = 0 }

initialResults = Results 0 0 5 200 3

main = getStdGen >>= evalRandT playGame

playGame :: RandIO ()
playGame = do
    liftIO $ putStrLn "Try your hand at governing ancient Sumeria successfully for a 10-year term of office."
    liftIO $ printResults initialState initialResults
    finalState <- repeatN 10 doYear initialState
    printFinalReport finalState

doYear :: GameState -> RandIO GameState
doYear s0 = do
    s1 <- buyOrSellLand s0
    bushelsForFood <- liftIO $ askBushelsForFood s1
    acresToPlant <- liftIO $ askAcresToPlant s1 { food = (food s1) - bushelsForFood }
    (sOut, resultsOut) <- applyOrders bushelsForFood acresToPlant s1
    liftIO $ printResults sOut resultsOut
    let fractionStarved = (fromIntegral (peopleStarved resultsOut)) / (fromIntegral (people s0))
    when (fractionStarved > 0.45) $ liftIO $ do
        printf "You starved %d people in one year!\n" (peopleStarved resultsOut)
        putStr finkMessage
        exitSuccess
    return sOut

applyOrders :: MonadRandom m => Int -> Int -> GameState -> m (GameState, Results)
applyOrders bushelsEaten acresPlanted state = do

    birthD6 <- rollD6
    bushelsPerAcre <- rollD6
    plagueHappens <- rollProbability 0.15
    ratD6 <- rollD6

    let bushelsInit = food state
        bushelsPlanted = acresPlanted `div` 2
        bushelsBeforeRats = bushelsInit - (bushelsEaten + bushelsPlanted)
        bushelsEatenByRats = if (ratD6 `mod` 2) == 1
            then 0
            else bushelsBeforeRats `div` ratD6
        bushelsHarvested = bushelsPerAcre * acresPlanted
        bushelsFinal = (bushelsBeforeRats - bushelsEatenByRats) + bushelsHarvested

        peopleInit = people state
        peopleBorn = 1 + ((birthD6 * (20 * (land state) + bushelsFinal)) `div` (peopleInit * 100))
        peopleFed = min peopleInit (bushelsEaten `div` 20)
        peopleStarved = peopleInit - peopleFed
        peopleBeforePlague = peopleFed + peopleBorn
        peopleDiedOfPlague = if plagueHappens
            then peopleBeforePlague `div` 2
            else 0
        peopleFinal = peopleBeforePlague - peopleDiedOfPlague

        cumDeathRateOut = (cumDeathRate state) + (fromIntegral peopleStarved) /
            (fromIntegral peopleInit)

    return (
        state {
            year = (year state) + 1,
            people = peopleFinal,
            food = bushelsFinal,
            totalDeaths = (totalDeaths state) + peopleDiedOfPlague + peopleStarved,
            cumDeathRate = cumDeathRateOut },
        Results {
            peopleStarved = peopleStarved,
            peopleDiedOfPlague = peopleDiedOfPlague,
            peopleBorn = peopleBorn,
            bushelsEatenByRats = bushelsEatenByRats,
            bushelsPerAcre = bushelsPerAcre }
        )

buyOrSellLand :: GameState -> RandIO GameState
buyOrSellLand s = do
    landPrice <- getRandomR (17, 26)
    liftIO $ do
        printf "Land is trading at %d bushels per acre.\n" landPrice
        acresToBuy <- askAcresToBuy s landPrice
        acresToSell <- if acresToBuy > 0 then return 0 else askAcresToSell s landPrice
        let acresChange = acresToBuy - acresToSell
        return s {
            land = (land s) + acresChange,
            food = (food s) - landPrice * acresChange }

askAcresToBuy :: GameState -> Int -> IO Int
askAcresToBuy s landPrice = askNumber nMax nDefault promptMsg retryMsg helpMsg where
    promptMsg = "How many acres do you wish to buy"
    retryMsg = printf "You have only %d bushels of grain" (food s)
    helpMsg = buySellHelp s landPrice
    nMax = (food s) `div` landPrice
    nDefault = 0

askAcresToSell :: GameState -> Int -> IO Int
askAcresToSell s landPrice = askNumber nMax nDefault promptMsg retryMsg helpMsg where
    promptMsg = "How many acres do you wish to sell"
    retryMsg = printf "You have only %d acres" nMax
    helpMsg = buySellHelp s landPrice
    nMax = land s
    nDefault = 0

buySellHelp :: GameState -> Int -> String
buySellHelp s landPrice
    | acres > 0 = printf "For feeding and planting you need %d bushels, leaving enough to buy %d acres." foodNeeded acres
    | acres < 0 = printf "For feeding and planting you need %d bushels, which requires selling %d acres." foodNeeded (negate acres)
    | otherwise = printf "You have exactly enough food for feeding and planting."
    where
    foodNeeded = 25 * (people s) -- 20 bushels for eating and 5 for planting
    acres :: Int
    acres = floor $ fromIntegral ((food s) - foodNeeded) / fromIntegral landPrice

askBushelsForFood :: GameState -> IO Int
askBushelsForFood s = askNumber nMax nDefault promptMsg retryMsg helpMsg where
    promptMsg = "How many bushels do you wish to feed your people"
    retryMsg = printf "You have only %d bushels of grain" (food s)
    helpMsg = "Each person needs 20 bushels of grain for the year to not starve."
    nMax = food s
    nDefault = min nMax (20 * (people s))

askAcresToPlant :: GameState -> IO Int
askAcresToPlant s = askNumber nMax nDefault promptMsg retryMsg helpMsg where
    promptMsg = "How many acres do you wish to plant with seed"
    (nMax, retryMsg) = minimumBy (comparing fst) [
        (land s, printf "You own only %d acres" (land s)),
        (2 * (food s), printf "You have only %d bushels of grain" (food s)),
        (10 * (people s), printf "You have only %d people to tend the fields" (people s)) ]
    helpMsg = "One bushel of food can plant two acres; one person can tend ten acres."
    nDefault = nMax

askNumber :: Int -> Int -> String -> String -> String -> IO Int
askNumber nMax nDefault promptMsg retryMsg helpMsg =
    if nMax <= 0 then
        return 0
    else do
        putStr $ printf "%s (0-%d)? [%d] " promptMsg nMax nDefault
        hFlush stdout
        line <- getLine
        case line of
            "" -> return nDefault
            "q" -> exitSuccess
            "h" -> do
                putStrLn helpMsg
                askNumber nMax nDefault promptMsg retryMsg helpMsg
            otherwise -> case readMaybe line of
                Nothing -> do
                    putStrLn "Hammurabi: I did not understand that number. Now then,"
                    askNumber nMax nDefault promptMsg retryMsg helpMsg
                Just n ->
                    if n < 0 then do
                        putStrLn "Hammurabi: I cannot do what you wish!"
                        putStrLn "Get yourself another steward!!!!!"
                        exitSuccess
                    else if n > nMax then do
                        putStrLn $ printf "Hammurabi: Think again. %s. Now then," retryMsg
                        askNumber nMax nDefault promptMsg retryMsg helpMsg
                    else
                        return n

printResults :: GameState -> Results -> IO ()
printResults state results = do
    printf "\nHamurabi: I beg to report to you,\n"
    printf "In year %d, %d people starved, %d came to the city.\n" (year state) (peopleStarved results) (peopleBorn results)
    printf "%s" (if (peopleDiedOfPlague results) > 0 then "A horrible plague struck! Half the people died.\n" else "")
    printf "Population is now %d.\n" (people state)
    printf "The city now owns %d acres.\n" (land state)
    when ((land state) > 0) $
        printf "You harvested %d bushels per acre.\n" (bushelsPerAcre results)
    printf "Rats ate %d bushels.\n" (bushelsEatenByRats results)
    printf "You now have %d bushels in store.\n" (food state)

printFinalReport :: GameState -> RandIO ()
printFinalReport s = do
    let numPeople = people s
    numHaters <- getRandomR (0, (numPeople * 4) `div` 5)
    let numYears = year s
        numAcres = land s
        numDeaths = totalDeaths s
        avgDeathRate = (cumDeathRate s) / (fromIntegral numYears)
        acresPerPerson = (fromIntegral numAcres) / (fromIntegral numPeople)
        comments
            | avgDeathRate > 0.33 || acresPerPerson < 7 = finkMessage
            | avgDeathRate > 0.1 || acresPerPerson < 9 =
                "Your heavy-handed performance smacks of Nero and Ivan IV.\n" ++
                "The people (remaining) find you an unpleasant ruler, and,\n" ++
                "frankly, hate your guts!\n"
            | avgDeathRate > 0.03 || acresPerPerson < 10 =
                "Your performance could have been somewhat better, but\n" ++
                "really wasn't too bad at all. " ++
                show numHaters ++ " people would\n" ++
                "dearly like to see you assassinated but we all have our\n" ++
                "trivial problems.\n"
            | otherwise =
                "A fantastic performance!!! Charlemagne, Disraeli, and\n" ++
                "Jefferson combined could not have done better!\n"
        msg = "In your " ++ show numYears ++ "-year term of office, " ++
            show (round (100.0 * avgDeathRate)) ++ " percent of the\n" ++
            "population starved per year on average, i.e., " ++
            "a total of " ++ show numDeaths ++ " people died!!\n" ++
            "You started with 10 acres per person and ended with " ++
            show (round acresPerPerson) ++ " acres per person.\n" ++
            comments
    liftIO $ do
        putStr msg
        putStrLn "So long for now."

finkMessage :: String
finkMessage =
    "Due to this extreme mismanagement you have not only\n" ++
    "been impeached and thrown out of office but you have\n" ++
    "also been declared 'National Fink' !!\n"

rollD6 :: MonadRandom m => m Int
rollD6 = getRandomR (1, 6)

rollProbability :: MonadRandom m => Double -> m Bool
rollProbability probability = fmap (< probability) getRandom

repeatN :: (Monad m) => Int -> (a -> m a) -> a -> m a
repeatN n f x
    | n <= 0 = return x
    | otherwise = f x >>= repeatN (n - 1) f

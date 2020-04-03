import Test.QuickCheck
import Control.Monad
import System.Exit
import Bowling

data Frame = Average Int Int
    deriving (Eq, Show)

instance Arbitrary Frame
    where
    arbitrary = do
        throw1 <- choose (0,9)
        throw2 <- choose (0, 9-throw1)
        return (Average throw1 throw2)

averageGame :: Gen [Frame]
averageGame = do
    nbFrames <- choose (0,10)
    sequence $ replicate nbFrames (arbitrary :: Gen Frame)

toThrows :: Frame -> [Throw]
toThrows (Average throw1 throw2) = [throw1, throw2]

check s prop = do
    putStr ("\n" ++ s ++ ": ")
    result <- quickCheckResult prop
    unless (isSuccess result) exitFailure

main :: IO ()
main = do
    check "all bowling games have their score <= 300" prop_max_score
    check "an average game gets only the sum of throws" (forAll averageGame prop_average_game)


prop_max_score ts = score ts <= 300


prop_average_game game = 
    let throws = concatMap toThrows game in score throws == sum throws 

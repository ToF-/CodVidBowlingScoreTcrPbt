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


toThrows :: Frame -> [Throw]
toThrows (Average throw1 throw2) = [throw1, throw2]

check s prop = do
    putStr ("\n" ++ s ++ ": ")
    result <- quickCheckResult prop
    unless (isSuccess result) exitFailure

main :: IO ()
main = do
    check "all bowling games have their score <= 300" prop_max_score
    check "an average game gets only the sum of throws" prop_average_game


prop_max_score ts = score ts <= 300

prop_average_game :: [Frame] -> Bool
prop_average_game frames = score throws == sum throws 
    where throws = concatMap toThrows (take 10 frames)

import Test.QuickCheck
import Control.Monad
import System.Exit
import Bowling

check s prop = do
    putStr ("\n" ++ s ++ ": ")
    result <- quickCheckResult prop
    unless (isSuccess result) exitFailure

main :: IO ()
main = check "all bowling games have their score <= 300" prop_max_score

prop_max_score ts = score ts <= 300

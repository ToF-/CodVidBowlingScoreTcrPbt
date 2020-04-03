import Test.QuickCheck
import Control.Monad
import System.Exit

check s prop = do
    putStr ("\n" ++ s ++ ": ")
    result <- quickCheckResult prop
    unless (isSuccess result) exitFailure

main :: IO ()
main = check "dummy" prop_dummy

prop_dummy b = b == not (not b)

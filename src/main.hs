
import System.Environment
import Data.Char


fib :: (Integral a) => a -> a
fib 0 = 1
fib 1 = 1
fib x = fib (x - 1) + fib (x - 2)

main :: IO ()
main = do
    print $ fib 15

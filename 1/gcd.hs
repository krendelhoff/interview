import           Prelude            hiding (gcd)
import           System.Environment (getArgs)

gcd :: Integral a => a -> a -> a
gcd a 0 = a
gcd a b = gcd b (a `mod` b)

-- подать в качестве аргументов командной строки
main = do
  args <- getArgs
  case args of
    [n1, n2] -> do
      print $ gcd (read n1 :: Integer) (read n2)
    _ -> putStrLn "wrong args"

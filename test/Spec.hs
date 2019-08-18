import           Lib

main :: IO ()
main = do
  x <- putStrLn "\nSTARTING TEST\n"
  x <- someFunc
  x <- putStrLn "\nDONE\n"
  return ()

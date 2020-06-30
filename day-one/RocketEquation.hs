module RocketEquation where

fuelRequired :: Double -> Integer
fuelRequired x =  (floor $ x/3) - 2

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

main :: IO Integer
main = do
  lines <- readLines "/Users/joao.lanes/dev/Haskell/AoC/day-one/fuel.txt"
  let xs = fmap (\x -> read x :: Double) lines
  return (sum $ map fuelRequired xs)

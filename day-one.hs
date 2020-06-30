module RocketEquation where

fuelRequired :: Double -> Integer
fuelRequired x =  (floor $ x/3) - 2

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

pt1main :: IO Integer
pt1main = do
  lines <- readLines "/Users/joao.lanes/dev/Haskell/AoC/day-one/fuel.txt"
  let xs = fmap (\x -> read x :: Double) lines
  return (sum $ map fuelRequired xs)

recFuelRequired :: [Double] -> Integer -> Integer
recFuelRequired [] acc = acc
recFuelRequired (x:xs) acc
  | fuel > 0 = recFuelRequired ((fromIntegral fuel):xs) (acc + fuel)
  | otherwise = recFuelRequired xs acc
  where
    fuel = fuelRequired x

pt2main :: IO Integer
pt2main = do
  lines <- readLines "/Users/joao.lanes/dev/Haskell/AoC/day-one/fuel.txt"
  let totalFuel = recFuelRequired (map (\x -> read x :: Double) lines) 0
  return totalFuel

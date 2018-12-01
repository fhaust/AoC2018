

module Day1 where

import Data.List
import qualified Data.Set as Set

part1 :: [Int] -> Int
part1 = sum

part2 :: [Int] -> Int
part2 input = all !! firstRepeated
  where
    all = scanl (+) 0 (cycle input)
    repeated = snd . mapAccumL (\s a -> (Set.insert a s, Set.member a s)) Set.empty $ all
    (Just firstRepeated) = findIndex id repeated

main :: IO ()
main = do

  input <- map read . lines . filter (/= '+') <$> readFile "inputs/day01.txt"

  let result1 = part1 input
  let result2 = part2 input

  putStrLn $ "result1: " ++ show result1
  putStrLn $ "result2: " ++ show result2

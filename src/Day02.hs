

module Day02 where

import Data.List

part1 input = countSame (==2) input * countSame (==3) input
  where countSame f = length . filter (any (f . length)) . map (group . sort)

part2 input = intersect foundA foundB
  where dist a b = length . filter (uncurry (/=)) $ zip a b
        [foundA,foundB] = [ a | a <- input, b <- input, dist a b == 1 ]

main = do

  input <- lines <$> readFile "inputs/day02.txt"

  putStrLn $ "result part 1: " ++ show (part1 input) -- 5478
  putStrLn $ "result part 2: " ++ show (part2 input) -- qyzphxoiseldjrntfygvdmanu

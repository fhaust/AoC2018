

module Day03 where

import Data.List
import Data.List.Split

import qualified Data.Map as M

parseInput = map (toClaim . go) . lines
  where go = filter (not . null) . splitOneOf "#@ ,:x"


toClaim :: [String] -> M.Map (Int,Int) [Int]
toClaim [id, ox, oy, w, h] = M.fromList [((read ox+x, read oy+y),[read id])
                                        | x <- [0..read w-1], y <- [0..read h-1]]


part1 inputs = M.size . M.filter ((>= 2) . length) $ claims
  where claims = foldl' (M.unionWith (++)) M.empty $ inputs


part2 inputs = head . snd . M.findMax $ unclaimed -- unwrap that mess
  where (Just unclaimed) = find (\a -> all (\b -> M.null $ M.intersection a b) (filter (/= a) inputs)) inputs



main = do

  inputs <- parseInput <$> readFile "inputs/day03.txt"

  putStrLn $ "result 1: " ++ show (part1 inputs)
  putStrLn $ "result 2: " ++ show (part2 inputs)


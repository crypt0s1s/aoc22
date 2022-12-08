module Day3
  ( day3,
  )
where

import qualified Data.Set as S
import Data.Char ( ord, isLower )
import Data.List.Split ( chunksOf )

day3 :: IO ()
day3 = do
  inp <- lines <$> readFile "data/day3.txt"
  putStrLn $ "Day 3 - p1: " ++ (show $ p1 inp) ++ ", p2: " ++ (show $ p2 inp)

p1 :: [String] -> Int
p1 = foldr (\x -> (+) (findPriorityOfDup x)) 0 

findPriorityOfDup :: String -> Int
findPriorityOfDup xs = foldr (\x -> (+) (getPriorityFromChar x)) 0 intersection
  where
    (sec1,sec2) = splitString xs
    set1 = S.fromList sec1
    set2 = S.fromList sec2
    intersection = S.intersection set1 set2

splitString :: String -> (String, String)
splitString ls = splitAt (length ls `quot` 2) ls

getPriorityFromChar :: Char -> Int
getPriorityFromChar c | isLower c = ord c - 96
                      | otherwise = ord c - 38

p2 :: [String] -> Int
p2 xs = foldr (\x -> (+) (findBadgePriority x)) 0 chunks
  where 
    chunks = chunksOf 3 xs

findBadgePriority :: [String] -> Int
findBadgePriority group = foldr (\x -> (+) (getPriorityFromChar x)) 0 $ intersection sets
  where
    sets = map S.fromList group
    intersection [a, b, c] = foldr S.intersection a [b, c]
    intersection _ = error "There should be 3 sets present"

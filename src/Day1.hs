module Day1
  ( day1,
  )
where

import Data.List.Split

day1 :: IO ()
day1 = do
  inp <- lines <$> readFile "data/day1.txt"
  let gLines = splitOn [""] inp
  putStrLn $ "Day 1 - p1: " ++ (show $ p1 gLines) ++ ", p2: " ++ (show $ p2 gLines)

p1 :: [[String]] -> Int
p1 gLines = foldr (\x -> max (getSum x)) 0 gLines

p2 :: [[String]] -> Int
p2 gLines = a + b + c
  where
    (a, b, c) = getTop3 gLines

getTop3 :: [[String]] -> (Int, Int, Int)
getTop3 = foldr (\x y -> updateTop3 y (getSum x)) (0, 0, 0)

updateTop3 :: (Int, Int, Int) -> Int -> (Int, Int, Int)
updateTop3 (a, b, c) i
  | a <= b && a <= c && i > a = (i, b, c)
  | b <= c && i > b = (a, i, c)
  | i > c = (a, b, i)
  | otherwise = (a, b, c)

getSum :: [String] -> Int
getSum ls = foldr (\x -> (+) (read x)) 0 ls

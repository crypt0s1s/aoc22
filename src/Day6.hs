module Day6
( day6,
) where

import qualified Data.List as L
import qualified Data.Set as S

day6 :: IO ()
day6 = do
  inp <- readFile "data/day6.txt"
  putStrLn $ "Day 6 -  p1: " ++ (show $ p1 inp) ++ ", p2: " ++ (show $ p2 inp)

p1 :: String -> Int
p1 inp = i
  where
    thing = drop 3 $ L.zip5 [(1 :: Int)..] inp (' ':inp) (' ':' ':inp) (' ':' ':' ':inp)
    (i, _, _, _, _) = head $ dropWhile lettersNotUnique thing

lettersNotUnique :: (Int, Char, Char, Char, Char) -> Bool
lettersNotUnique (_, a, b, c, d) = not $ a /= b && a /= c && a /= d && b /= c && b /= d && c /= d

p2 :: String -> Int
p2 inp = p2' 14 inp

p2' :: Int -> String -> Int
p2' n inp | isNextNCharUnique 14 inp = n
          | otherwise = p2' (n + 1) (tail inp)

isNextNCharUnique :: Int -> String -> Bool
isNextNCharUnique n ls = S.size set == n
  where
    set = S.fromList $ take n ls

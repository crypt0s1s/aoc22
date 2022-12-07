module Day4
  ( day4,
  )
where

import Data.Char

type Assignment = (Int, Int)

(+++) :: (Int, Int) -> (Int, Int) -> (Int, Int)
(a, b) +++ (a', b') = (a + a', b + b')

day4 :: IO ()
day4 = do
  inp <- lines <$> readFile "data/day4.txt"
  let (p1, p2) = foldr (\x -> (+++) (findSubsetsAndOverlap x)) (0, 0) inp
  putStrLn $ "Day 4 -  p1: " ++ (show p1) ++ ", p2: " ++ (show p2)

findSubsetsAndOverlap :: String -> (Int, Int)
findSubsetsAndOverlap xs = (isAssignmentSubset assignment, isAssignmentOverlap assignment)
  where
    assignment = parseAssignment xs

isAssignmentSubset :: (Assignment, Assignment) -> Int
isAssignmentSubset ((a,b),(a',b')) | a >= a' && b <= b' = 1
                                   | a' >= a && b' <= b = 1
                                   | otherwise = 0

parseAssignment :: String -> (Assignment, Assignment)
parseAssignment xs = ((read a, read b), (read c, read d))
  where
    (a, r1) = span isDigit xs
    (b, r2) = span isDigit $ drop 1 r1
    (c, r3) = span isDigit $ drop 1 r2
    (d, _) = span isDigit $ drop 1 r3

isAssignmentOverlap :: (Assignment, Assignment) -> Int
isAssignmentOverlap ((a,b),(a',b')) | a >= a' && a <= b' = 1
                                    | b >= a' && b <= b' = 1
                                    | a' >= a && a' <= b = 1
                                    | b' >= a && b' <= b = 1
                                    | otherwise = 0

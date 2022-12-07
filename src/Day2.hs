module Day2
  ( day2,
  )
where

data Shape = Rock | Paper | Scissors deriving Eq

data Outcome = Win | Loss | Draw

day2 :: IO ()
day2 = do
  inp <- lines <$> readFile "data/day2.txt"
  let (p1, p2) = foldr (\x -> (+++) (computeScores $ toShapes x)) (0, 0) inp
  putStrLn $ "Day 2 - p1: " ++ show p1 ++ ", p2: " ++ show p2

(+++) :: (Int, Int) -> (Int, Int) -> (Int, Int)
(a, b) +++ (a', b') = (a + a', b + b')

toShapes :: String -> (Shape, Shape, Shape)
toShapes [o, _, p] = (oppenentShape, toPlayerShapeP1 p, toPlayerShapeP2 oppenentShape p)
  where oppenentShape = toOpponentShape o
toShapes x = error $ "Should never reach this point. String: " ++ show x

toOpponentShape :: Char -> Shape
toOpponentShape 'A' = Rock
toOpponentShape 'B' = Paper
toOpponentShape 'C' = Scissors
toOpponentShape s = error $ "Invalid opponenet shape: " ++ show s

toPlayerShapeP1 :: Char -> Shape
toPlayerShapeP1 'X' = Rock
toPlayerShapeP1 'Y' = Paper
toPlayerShapeP1 'Z' = Scissors
toPlayerShapeP1 s = error $ "Invalid player shape: " ++ show s

toPlayerShapeP2 :: Shape -> Char -> Shape
toPlayerShapeP2 s o = computeShape s outcome
  where
    outcome = getOutcome o

computeScores :: (Shape, Shape, Shape) -> (Int, Int)
computeScores (opponentShape, playerShapeP1, playerShapeP2) = (shapeScoreP1 + outcomeScoreP1, shapeScoreP2 + outcomeScoreP2)
  where
    shapeScoreP1 = getScoreShape playerShapeP1
    outcomeScoreP1 = getScoreOutcome $ computeOutcome opponentShape playerShapeP1
    shapeScoreP2 = getScoreShape playerShapeP2
    outcomeScoreP2 = getScoreOutcome $ computeOutcome opponentShape playerShapeP2

computeOutcome :: Shape -> Shape -> Outcome
computeOutcome s s' | s == s' = Draw
computeOutcome Rock Paper = Win
computeOutcome Rock Scissors = Loss
computeOutcome Paper Scissors = Win
computeOutcome Paper Rock = Loss
computeOutcome Scissors Rock = Win
computeOutcome Scissors Paper = Loss
computeOutcome _ _ = error "Unexpected compute outcome"

computeShape :: Shape -> Outcome -> Shape
computeShape s Draw = s
computeShape Rock Loss = Scissors
computeShape Rock Win = Paper
computeShape Paper Loss = Rock
computeShape Paper Win = Scissors
computeShape Scissors Loss = Paper
computeShape Scissors Win = Rock

getOutcome :: Char -> Outcome
getOutcome 'X' = Loss
getOutcome 'Y' = Draw
getOutcome 'Z' = Win
getOutcome e = error $ "Unknown outcome: " ++ show e

getScoreShape :: Shape -> Int
getScoreShape Rock = 1
getScoreShape Paper = 2
getScoreShape Scissors = 3

getScoreOutcome :: Outcome -> Int
getScoreOutcome Win = 6
getScoreOutcome Loss = 0
getScoreOutcome Draw = 3

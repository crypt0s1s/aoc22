module Day5
  ( day5,
  )
where

import Data.Char
import qualified Data.List as L
import qualified Data.Map.Lazy as M

type Amount = Int
type From = Int
type To = Int
type Crate = Char
type CrateStackMap = M.Map Int CrateStack

data Action = Action Amount From To
data CrateStack = Stack Crate CrateStack | Floor deriving Show
data Direction = DL | DR

day5 :: IO ()
day5 = do
  inp <- lines <$> readFile "data/day5.txt"
  let (p1, p2) = moveCrates $ parseInput inp
  putStrLn $ "Day 5 - p1: " ++ (show p1) ++ ", p2: " ++ (show p2)

moveCrates :: (CrateStackMap, [Action]) -> (String, String)
moveCrates (crateStacks, actions) = (p1Result, p2Result)
  where
    p1FinalStack = foldl (performAction DR) crateStacks actions 
    p1Result = map getTopCrate $ M.elems p1FinalStack
    p2FinalStack = foldl (performAction DL) crateStacks actions 
    p2Result = map getTopCrate $ M.elems p2FinalStack

getTopCrate :: CrateStack -> Char
getTopCrate (Stack c _) = c
getTopCrate _ = error "Stack should not be empty"

performAction :: Direction -> CrateStackMap -> Action -> CrateStackMap
performAction dir stacks (Action amount from to) = stacks''
  where
    stackFrom = stacks M.! from
    stackTo = stacks M.! to
    (stackFrom', takenCrates) = takeXFromStack stackFrom amount
    stackTo' = placeCratesOnStack dir stackTo takenCrates
    stacks' = M.insert from stackFrom' stacks 
    stacks'' = M.insert to stackTo' stacks'

takeXFromStack :: CrateStack -> Int -> (CrateStack, [Crate])
takeXFromStack stack i = (newStack, takenCrates)
  where
    (newStack, takenCrates) = foldr (\_ -> take1FromStack) (stack, []) [1..i]
    take1FromStack :: (CrateStack, [Crate]) -> (CrateStack, [Crate])
    take1FromStack (Stack crate crateStack, crates) = (crateStack, crate:crates)
    take1FromStack (Floor, _) = error "No crates available to be taken"

placeCratesOnStack :: Direction -> CrateStack -> [Crate] -> CrateStack
placeCratesOnStack DR stack crates = foldr (\c s -> Stack c s) stack crates
placeCratesOnStack DL stack crates = foldl (\s c -> Stack c s) stack crates

parseInput :: [String] -> (CrateStackMap, [Action])
parseInput ls = (crateStacks, actions)
  where
    (rawCrates, rawActions) = splitAt 8 ls
    crateStacks = parseCrates rawCrates
    actions = map parseAction $ drop 2 rawActions

parseAction :: String -> Action
parseAction action = Action (read amount) (read from) (read to)
  where
    (amount, r1) = spanDigits $ dropNonDigits action
    (from, r2) = spanDigits $ dropNonDigits r1
    (to, _) = spanDigits $ dropNonDigits r2
    dropNonDigits ls = dropWhile (not . isDigit) ls
    spanDigits xs = span isDigit xs

parseCrates :: [String] -> CrateStackMap
parseCrates xs = M.fromList $ zip [1..] stacks
  where
    transposedCrates = tail $ L.transpose xs
    takeEvery3 = [ stack | (stack, i) <- zip transposedCrates ([0..] :: [Int]), i `mod` 4 == 0]
    stacks = map (\x -> toCrateStack $ dropWhile ((==) ' ') x) takeEvery3
    toCrateStack :: String -> CrateStack
    toCrateStack ls = foldr Stack Floor ls
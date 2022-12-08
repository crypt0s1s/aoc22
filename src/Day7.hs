module Day7
( day7,
) where

data Command = LS Int | CD String

day7 :: IO ()
day7 = do
  inp <- lines <$> readFile "data/day7.txt"
  let commands = parseCommands inp
  let sizeList = createSizeList commands
  putStrLn $ "Day 7 -  p1: " ++ (show $ p1 sizeList) ++ ", p2: " ++ (show $ p2 sizeList)

p1 :: [Int] -> Int
p1 = foldr sumSizesLessThan 0
  where
    sumSizesLessThan :: Int -> Int -> Int
    sumSizesLessThan s total | s > 100000 = total
                             | otherwise = s + total

p2 :: [Int] -> Int
p2 sizes = foldl updateIfBestFit 70000000 sizes
  where
    extraSpaceReq = 30000000 - (70000000 - (head sizes))
    updateIfBestFit :: Int -> Int -> Int
    updateIfBestFit currentBestSize candidateSize | candidateSize >= extraSpaceReq && candidateSize < currentBestSize = candidateSize
                                                  | otherwise = currentBestSize

createSizeList :: [Command] -> [Int]
createSizeList commands = goBackToRootSize $ foldl createSizeList' ([],[]) commands

createSizeList' :: ([Int], [Int]) -> Command -> ([Int], [Int])
createSizeList' (sl, ps1:ps2:ps) (CD "..") = (ps1:sl, (ps2 + ps1):ps)
createSizeList' (sl, ps) (CD _) = (sl, ps)
createSizeList' (sl, ps) (LS directFolderSize) = (sl, directFolderSize:ps)

goBackToRootSize :: ([Int], [Int]) -> [Int]
goBackToRootSize (sl, ps) = fst $ foldl goBackToRootSize' (sl, ps) ps

goBackToRootSize' :: ([Int], [Int]) -> Int -> ([Int],[Int])
goBackToRootSize' (sl,ps1:ps2:ps) _ = (ps1:sl, (ps2 + ps1):ps)
goBackToRootSize' (sl,[p]) _ = (p:sl, [])
goBackToRootSize' _ _ = error "Should never reach"

parseCommands :: [String] -> [Command]
parseCommands [] = []
parseCommands (l:ls) | t4 == "$ cd" = (parseCD l : parseCommands ls)
                     | t4 == "$ ls" = (lsCommand: parseCommands leftovers)
                     | otherwise = error $ "Unknown command " ++ l
  where
    (leftovers, lsCommand) = parseLS ls
    t4 = take 4 l

parseCD :: String -> Command
parseCD ls = CD dir
  where
    dir = drop 5 ls

parseLS :: [String] -> ([String], Command)
parseLS ls = (remainingLines, LS directFolderSize)
  where
    (folderContents, remainingLines) = span (\x -> head x /= '$') ls
    directFolderSize = foldr (\x -> (+) (parseObject x)) 0 folderContents

parseObject :: String -> Int
parseObject object | typeOrSize == "dir" = 0
                   | otherwise = read typeOrSize
  where
    typeOrSize = takeWhile (\x -> x /= ' ') object

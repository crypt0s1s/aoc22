module Day7
( day7,
) where

day7 :: IO ()
day7 = do
  inp <- lines <$> readFile "data/day7.txt"
  let sizeList' = parseAndProcessCommands ([],[]) inp
  putStrLn $ "Day 7 - p1: " ++ (show $ p1 sizeList') ++ ", p2: " ++ (show $ p2 sizeList')

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

goBackToRootSize :: ([Int], [Int]) -> [Int]
goBackToRootSize (sl, ps) = fst $ foldl goBackToRootSize' (sl, ps) ps

goBackToRootSize' :: ([Int], [Int]) -> Int -> ([Int],[Int])
goBackToRootSize' (sl,ps1:ps2:ps) _ = (ps1:sl, (ps2 + ps1):ps)
goBackToRootSize' (sl,[p]) _ = (p:sl, [])
goBackToRootSize' _ _ = error "Should never reach"

processLS :: ([Int],[Int]) -> Int -> ([Int],[Int])
processLS (sl, ps) directFolderSize = (sl, directFolderSize:ps)

processCD :: ([Int],[Int]) -> String -> ([Int],[Int])
processCD (sl, ps1:ps2:ps) cdCommand | dest == ".." = (ps1:sl, (ps2 + ps1):ps)
                                     | otherwise = (sl, ps1:ps2:ps)
  where
    dest = drop 5 cdCommand
processCD (sl, ps) _ = (sl, ps)

parseAndProcessCommands :: ([Int], [Int]) -> [String] -> [Int]
parseAndProcessCommands s [] = goBackToRootSize s
parseAndProcessCommands s (l:ls) | t4 == "$ cd" = parseAndProcessCommands (processCD s l) ls
                                 | t4 == "$ ls" = parseAndProcessCommands (processLS s directFolderSize) leftovers
                                 | otherwise = error $ "Unknown command " ++ l
  where
    (leftovers, directFolderSize) = getDirectFolderSize ls
    t4 = take 4 l

getDirectFolderSize :: [String] -> ([String], Int)
getDirectFolderSize ls = (remainingLines, directFolderSize)
  where
    (folderContents, remainingLines) = span (\x -> head x /= '$') ls
    directFolderSize = foldr (\x -> (+) (parseObject x)) 0 folderContents

parseObject :: String -> Int
parseObject object | typeOrSize == "dir" = 0
                   | otherwise = read typeOrSize
  where
    typeOrSize = takeWhile (\x -> x /= ' ') object

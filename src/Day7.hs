module Day7
( day7,
) where

data Object = Dir String | File Int String deriving Show
data Command = LS [Object] | CD String
data DirectoryData = DirectoryData {
  dirRoot :: String,
  dirSize :: Int
} deriving Show

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
createSizeList' (sl, ps) (LS objects) = (sl, (getDirectFolderSize objects):ps)

goBackToRootSize :: ([Int], [Int]) -> [Int]
goBackToRootSize (sl, ps) = fst $ foldl goBackToRootSize' (sl, ps) ps

goBackToRootSize' :: ([Int], [Int]) -> Int -> ([Int],[Int])
goBackToRootSize' (sl,ps1:ps2:ps) _ = (ps1:sl, (ps2 + ps1):ps)
goBackToRootSize' (sl,[p]) _ = (p:sl, [])
goBackToRootSize' _ _ = error "Should never reach"

getDirectFolderSize :: [Object] -> Int
getDirectFolderSize objects = foldr (\x -> (+) (getDirectSize x)) 0 objects
  where
    getDirectSize :: Object -> Int
    getDirectSize (Dir _) = 0
    getDirectSize (File size _) = size

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
parseLS ls = (remainingLines, LS objects)
  where
    (folderContents, remainingLines) = span (\x -> head x /= '$') ls
    objects = map parseObject folderContents

parseObject :: String -> Object
parseObject object | typeOrSize == "dir" = Dir name
                   | otherwise = File (read typeOrSize) name
  where
    (typeOrSize, name) = span (\x -> x /= ' ') object

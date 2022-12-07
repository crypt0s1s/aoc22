module Day7
( day7,
) where

import qualified Data.Map.Lazy as M

data Object = Dir String | File Int String deriving Show
data Command = LS [Object] | CD String
data DirectoryData = DirectoryData {
  dirRoot :: String,
  dirSize :: Int
} deriving Show

type DirectoryMap = M.Map String DirectoryData

day7 :: IO ()
day7 = do
  inp <- lines <$> readFile "data/day7.txt"
  let commands = parseCommands inp
  let dMap = createDirectoryMap commands
  putStrLn $ "Day 7 -  p1: " ++ (show $ p1 dMap) ++ ", p2: " ++ (show $ p2 dMap)

p1 :: DirectoryMap -> Int
p1 dMap = foldr sumDirectories 0 dMap
  where
    sumDirectories :: DirectoryData -> Int -> Int
    sumDirectories dir ts | dirSize dir > 100000 = ts
                          | otherwise = dirSize dir + ts

p2 :: DirectoryMap -> Int
p2 dMap = findDirToDeleteSize
  where
    spaceUsed = dirSize $ dMap M.! "/"
    extraSpaceRequired = 30000000 - (70000000 - spaceUsed)
    findDirToDeleteSize = M.foldl (updateSizeIfDirMeetsCriteria extraSpaceRequired) 70000000 dMap

updateSizeIfDirMeetsCriteria :: Int -> Int -> DirectoryData -> Int
updateSizeIfDirMeetsCriteria spaceReq s dData | size >= spaceReq && size < s = size
                                              | otherwise = s
  where
    size = dirSize dData

createDirectoryMap :: [Command] -> DirectoryMap
createDirectoryMap commands = goBackToRoot $ foldl createDirectoryMap' (M.empty, []) commands

goBackToRoot :: (DirectoryMap, [String]) -> DirectoryMap
goBackToRoot (dMap, []) = dMap
goBackToRoot (dMap, (d:dirs)) = goBackToRoot (M.adjust (updateSize size) (dirRoot dirData) dMap, dirs)
  where
    dirData = dMap M.! (unwords $ d:dirs)
    size = dirSize dirData

createDirectoryMap' :: (DirectoryMap, [String]) -> Command -> (DirectoryMap, [String])
createDirectoryMap' (dMap, (d:dirs)) (CD "..") = (M.adjust (updateSize size) (dirRoot dirData) dMap, dirs)
  where
    dirData = dMap M.! (unwords $ d:dirs)
    size = dirSize dirData

createDirectoryMap' (dMap, dirs) (CD x) = (M.insert (unwords $ x:dirs) directoryData dMap, x:dirs)
  where
    directoryData = DirectoryData (unwords dirs) 0

createDirectoryMap' (dMap, dirs) (LS objects) = (M.adjust (updateSize directFolderSize) (unwords dirs) dMap, dirs)
  where
    directFolderSize = getDirectFolderSize objects

getDirectFolderSize :: [Object] -> Int
getDirectFolderSize objects = foldr (\x -> (+) (getDirectSize x)) 0 objects
  where
    getDirectSize :: Object -> Int
    getDirectSize (Dir _) = 0
    getDirectSize (File size _) = size

updateSize :: Int -> DirectoryData -> DirectoryData
updateSize size dirData = DirectoryData root newSize
  where
    root = dirRoot dirData
    newSize = size + dirSize dirData

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

module Main (main) where

import Day1
import Day2
import Day3
import Day4
import Day5
import Day6
import Day7
import System.Clock

main :: IO ()
main = do
  startTime <- getTime Monotonic
  day1
  day2
  day3
  day4
  day5
  day6
  day7
  endTime <- getTime Monotonic
  putStrLn $ "Total time: " ++ (show $ (toNanoSecs $ diffTimeSpec startTime endTime) `quot` 1000)

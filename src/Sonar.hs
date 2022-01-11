module Sonar
  ( readMeasurements
  , countIncreases
  , slidingWindows
  ) where

import Data.List


readMeasurements :: String -> IO [Int]
readMeasurements filePath = do
    content <- readFile filePath
    return $ map read $ lines content

slidingWindows :: Int -> [a] -> [[a]]
slidingWindows windowSize =
    foldr (zipWith (:)) (repeat []) . take windowSize . tails

hasIncrease :: [Int] -> Bool
hasIncrease [] = False
hasIncrease (x:xs) =
    let (isSorted, max, _last) = foldl step (True, x, x) xs
    in isSorted && max > x
  where
    step :: (Bool, Int, Int) -> Int -> (Bool, Int, Int)
    step (isSorted, max, prev) curr =
        (prev <= curr, if curr > prev then curr else prev, curr)

countIncreases :: [Int] -> Int
countIncreases =
    length . filter hasIncrease . slidingWindows 2

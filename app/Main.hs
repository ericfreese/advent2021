module Main (main) where

import qualified Options
import qualified Sonar
import qualified Submarine


main :: IO ()
main = do
    opts <- Options.parse
    solution opts

solution :: (Int, Int) -> IO ()
solution (1, 1) = do
    measurements <- Sonar.readMeasurements "data/sonar"
    print $ Sonar.countIncreases measurements

solution (1, 2) = do
    measurements <- Sonar.readMeasurements "data/sonar"
    let windows = Sonar.slidingWindows 3 measurements
    let windowSums = map sum windows
    print $ Sonar.countIncreases windowSums

solution (2, 1) = do
    course <- Submarine.readCourse "data/submarine-course"
    let (position, depth, _aim) = Submarine.followV1 course
    print $ position * depth

solution (2, 2) = do
    course <- Submarine.readCourse "data/submarine-course"
    let (position, depth, _aim) = Submarine.followV2 course
    print $ position * depth

solution (day, part) = do
    putStrLn ("No solution for day " ++ show day ++ ", part " ++ show part)

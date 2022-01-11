module Main (main) where

import qualified Options


main :: IO ()
main = do
    opts <- Options.parse
    solution opts

solution :: (Int, Int) -> IO ()
solution (day, part) = do
    putStrLn ("No solution for day " ++ show day ++ ", part " ++ show part)

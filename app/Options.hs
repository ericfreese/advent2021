module Options (parse) where

import Options.Applicative
import Data.Semigroup ((<>))


description :: String
description = "Generate solutions for 2021 Advent of Code"

data Opts = Opts
    { optsDay :: Int
    , optsPart :: Int
    }

optsParser :: Parser Opts
optsParser = Opts
    <$> option auto
        (long "day"
        <> short 'd'
        <> metavar "INT"
        <> help "Day to generate solution for")
    <*> option auto
        (long "part"
        <> short 'p'
        <> help "Part to generate solution for"
        <> metavar "INT")

parse :: IO (Int, Int)
parse = do
    opts <- execParser optsParserInfo
    return (optsDay opts, optsPart opts)
  where
    optsParserInfo = info (optsParser <**> helper) (progDesc description)

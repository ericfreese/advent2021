module Submarine
    ( readCourse
    , followV1
    , followV2
    ) where


type State = (Int, Int, Int)

type Command = (String, Int)

readCourse :: String -> IO [Command]
readCourse filePath = do
    content <- readFile filePath
    return $ map parseCommand $ lines content
  where
    parseCommand :: String -> Command
    parseCommand str =
        case words str of
            [action, amount] -> (action, read amount)

runCommandV1 :: State -> Command -> State
runCommandV1 (position, depth, aim) ("forward", amount) =
    (position + amount, depth, aim)

runCommandV1 (position, depth, aim) ("down", amount) =
    (position, depth + amount, aim)

runCommandV1 (position, depth, aim) ("up", amount) =
    (position, depth - amount, aim)

runCommandV2 :: State -> Command -> State
runCommandV2 (position, depth, aim) ("forward", amount) =
    (position + amount, depth + aim * amount, aim)

runCommandV2 (position, depth, aim) ("down", amount) =
    (position, depth, aim + amount)

runCommandV2 (position, depth, aim) ("up", amount) =
    (position, depth, aim - amount)

follow :: (State -> Command -> State) -> [Command] -> State
follow commandRunner = foldl commandRunner (0, 0, 0)

followV1 :: [Command] -> State
followV1 = follow runCommandV1

followV2 :: [Command] -> State
followV2 = follow runCommandV2

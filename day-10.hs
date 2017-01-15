import Text.Regex (mkRegex, matchRegex, Regex)
import qualified Data.Map as M
import Data.Maybe (isJust, fromMaybe)

data ChipType =
      High
    | Low
    deriving (Show)

data Destination =
      Bot Int
    | Output Int
    deriving (Show)

type Instruction = (Int, ChipType, Destination)

data Robot = Robot { high         :: Maybe Int
                   , low          :: Maybe Int
                   } deriving (Show)

type Robots = M.Map Int Robot

valueGoes :: Regex
valueGoes = mkRegex "value ([0-9]*) goes to bot ([0-9]*)"

botGives :: Regex
botGives = mkRegex "bot ([0-9]*) gives ([a-z]*) to ([a-z]*) ([0-9]*) and ([a-z]*) to ([a-z]*) ([0-9]*)"

maybeAddRobotWithValue :: Destination -> Maybe Int -> Robots -> Robots
maybeAddRobotWithValue (Bot n) cv rs =
    case M.lookup n rs of
    Nothing    -> M.insert n (Robot cv Nothing) rs
    Just robot -> case cv of
                  Nothing -> rs
                  Just v  -> M.update (addValue v) n rs
maybeAddRobotWithValue _       _ rs = rs

addValue :: Int -> Robot -> Maybe Robot
addValue v r =
    case high r of
    Nothing -> Just $ r { high = Just v }
    Just h  -> if v > h
               then Just $ r { high = Just v, low = Just h }
               else Just $ r { low = Just v }

stringToInstructions :: [String] -> (Int, (ChipType, Destination), (ChipType, Destination))
stringToInstructions [from, cType1, toType1, toNum1, cType2, toType2, toNum2] =
    let
        from'    = read from :: Int
        cType1'  = if cType1 == "low" then Low else High
        toNum1'  = read toNum1 :: Int
        toType1' = if toType1 == "bot" then Bot toNum1' else Output toNum1'
        cType2'  = if cType2 == "low" then Low else High
        toNum2'  = read toNum2 :: Int
        toType2' = if toType2 == "bot" then Bot toNum2' else Output toNum2'
    in
        (from', (cType1', toType1'), (cType2', toType2'))

makeRobotsAndIns :: [String] -> Robots -> [Instruction] -> (Robots, [Instruction])
makeRobotsAndIns s rs ins =
    let
        (from, ins1, ins2) = stringToInstructions s
        f                  = flip maybeAddRobotWithValue Nothing
        g                  =   (f $ snd ins2)
                             . (f $ snd ins1)
                             . (f $ Bot from)
    in
        (g rs, (from, fst ins1, snd ins1) : (from, fst ins2, snd ins2) : ins)

addInstruction :: String -> Robots -> [Instruction] -> (Robots, [Instruction])
addInstruction s rs ins =
    case matchRegex valueGoes s of
    Nothing         -> case matchRegex botGives s of
                       Nothing  -> (rs, ins)
                       Just x   -> makeRobotsAndIns x rs ins
    Just [value, n] -> (maybeAddRobotWithValue (Bot (read n :: Int)) (Just (read value :: Int)) rs, ins)
    _               -> (rs, ins)

makeRobots :: Robots -> [Instruction] -> Int -> IO (Robots, [Instruction])
makeRobots robots ins 0         = return (robots, ins)
makeRobots robots ins lineCount = do
    line <- getLine
    let (rs, ins') = addInstruction line robots ins
    makeRobots rs ins' (lineCount - 1)

applyValue :: ChipType -> Maybe Int -> Maybe Int -> Robot -> Maybe Robot
applyValue cType (Just h) (Just l) robot =
    case cType of
    High -> addValue h robot
    Low  -> addValue l robot

hasBothChips :: Robot -> Maybe (Maybe Int, Maybe Int)
hasBothChips r =
    let
        h = high r
        l = low r
    in
        if isJust h && isJust l
        then Just (h, l)
        else Nothing

runInstructions :: Robots -> [Instruction] -> Robots
runInstructions rs []      = rs
runInstructions rs (i@(n, cType, dest):ins) =
    case dest of
    Bot b -> case M.lookup n rs of
             Nothing    -> runInstructions rs ins
             Just robot -> case hasBothChips robot of
                           Nothing     -> runInstructions rs $ ins ++ [i]
                           Just (h, l) -> runInstructions (M.update (applyValue cType h l) b rs) ins
    _     -> runInstructions rs ins

filterRobots :: Int -> Int -> (Int, Robot) -> Bool
filterRobots h l (_, Robot{ high = rh, low = rl }) =
    h == (fromMaybe 0 rh) && l == (fromMaybe 0 rl)

readLines :: (Int -> IO (Robots, [Instruction])) -> IO (Robots, [Instruction])
readLines method = do
    lineCount <- getLine
    let n = read lineCount :: Int
    method n

main1 :: IO ()
main1 = do
    (rs, ins) <- readLines $ makeRobots M.empty []
    putStrLn . show $ filter (filterRobots 61 17) $ M.toList $ runInstructions rs ins
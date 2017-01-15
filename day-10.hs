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

data Place =
      PlaceR Robot
    | PlaceB Int
    deriving (Show)

type Places = M.Map String Place

valueGoes :: Regex
valueGoes = mkRegex "value ([0-9]*) goes to bot ([0-9]*)"

botGives :: Regex
botGives = mkRegex "bot ([0-9]*) gives ([a-z]*) to ([a-z]*) ([0-9]*) and ([a-z]*) to ([a-z]*) ([0-9]*)"

rId :: Int -> String
rId n = 'r' : show n

bId :: Int -> String
bId n = 'b' : show n

maybeAddRobotWithValue :: Destination -> Maybe Int -> Places -> Places
maybeAddRobotWithValue (Bot n)    cv ps =
    case M.lookup (rId n) ps of
    Nothing             -> M.insert (rId n) (PlaceR (Robot cv Nothing)) ps
    Just (PlaceR robot) -> case cv of
                           Nothing -> ps
                           Just v  -> M.update (addValue v) (rId n) ps
maybeAddRobotWithValue (Output n) cv ps =
    M.insert (bId n) (PlaceB $ fromMaybe 0 cv) ps

addValue :: Int -> Place -> Maybe Place
addValue v (PlaceR r) =
    case high r of
    Nothing -> Just $ PlaceR $ r { high = Just v }
    Just h  -> if v > h
               then Just $ PlaceR $ r { high = Just v, low = Just h }
               else Just $ PlaceR $ r { low = Just v }
addValue v (PlaceB n) = Just $ PlaceB v

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

makePlacesAndIns :: [String] -> Places -> [Instruction] -> (Places, [Instruction])
makePlacesAndIns s ps ins =
    let
        (from, ins1, ins2) = stringToInstructions s
        f                  = flip maybeAddRobotWithValue Nothing
        g                  =   (f $ snd ins2)
                             . (f $ snd ins1)
                             . (f $ Bot from)
    in
        (g ps, (from, fst ins1, snd ins1) : (from, fst ins2, snd ins2) : ins)

addInstruction :: String -> Places -> [Instruction] -> (Places, [Instruction])
addInstruction s ps ins =
    case matchRegex valueGoes s of
    Nothing         -> case matchRegex botGives s of
                       Nothing  -> (ps, ins)
                       Just x   -> makePlacesAndIns x ps ins
    Just [value, n] -> (maybeAddRobotWithValue (Bot (read n :: Int)) (Just (read value :: Int)) ps, ins)
    _               -> (ps, ins)

makePlaces :: Places -> [Instruction] -> Int -> IO (Places, [Instruction])
makePlaces robots ins 0         = return (robots, ins)
makePlaces robots ins lineCount = do
    line <- getLine
    let (ps, ins') = addInstruction line robots ins
    makePlaces ps ins' (lineCount - 1)

applyValue :: ChipType -> Maybe Int -> Maybe Int -> Place -> Maybe Place
applyValue cType (Just h) (Just l) place =
    case cType of
    High -> addValue h place
    Low  -> addValue l place

hasBothChips :: Robot -> Maybe (Maybe Int, Maybe Int)
hasBothChips r =
    let
        h = high r
        l = low r
    in
        if isJust h && isJust l
        then Just (h, l)
        else Nothing

runInstructions :: Places -> [Instruction] -> Places
runInstructions ps []      = ps
runInstructions ps (i@(n, cType, dest):ins) =
    case dest of
    Bot b    -> case M.lookup (rId n) ps of
                Nothing    -> runInstructions ps ins
                Just (PlaceR robot) -> case hasBothChips robot of
                                       Nothing     -> runInstructions ps $ ins ++ [i]
                                       Just (h, l) -> runInstructions (M.update (applyValue cType h l) (rId b) ps) ins
    Output o -> case M.lookup (rId n) ps of
                Nothing    -> runInstructions ps ins
                Just (PlaceR robot) -> case hasBothChips robot of
                                       Nothing     -> runInstructions ps $ ins ++ [i]
                                       Just (h, l) -> runInstructions (M.update (applyValue cType h l) (bId o) ps) ins

filterRobots :: Int -> Int -> (String, Place) -> Bool
filterRobots h l (_, (PlaceR Robot{ high = rh, low = rl })) =
    h == (fromMaybe 0 rh) && l == (fromMaybe 0 rl)
filterRobots _ _ _ = False

filterBins :: (Int -> Bool) -> (String, Place) -> Bool
filterBins f (['b', n], (PlaceB _)) = f (read [n] :: Int)
filterBins _ _                      = False

multBinChips :: (String, Place) -> Int -> Int
multBinChips (_, (PlaceB n)) total = n * total
multBinChips _               total = total

readLines :: (Int -> IO (Places, [Instruction])) -> IO (Places, [Instruction])
readLines method = do
    lineCount <- getLine
    let n = read lineCount :: Int
    method n

main1 :: IO ()
main1 = do
    (ps, ins) <- readLines $ makePlaces M.empty []
    putStrLn . show $ filter (filterRobots 61 17) $ M.toList $ runInstructions ps ins

main2 :: IO ()
main2 = do
    (ps, ins) <- readLines $ makePlaces M.empty []
    putStrLn . show $ foldr multBinChips 1 $ filter (filterBins (<=2)) $ M.toList $ runInstructions ps ins
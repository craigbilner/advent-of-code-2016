import Text.Regex (mkRegex, matchRegex, Regex)
import Data.List (nub, sortBy, groupBy)
import Data.Ord (comparing)

data Instruction =
      Rect Int Int
    | Row Int Int
    | Column Int Int

type X = Int

type Y = Int

type CoOrd = (X, Y)

type On = [CoOrd]

tryRegex :: String -> Regex -> Maybe [Int]
tryRegex str pat =
    case matchRegex pat str of
    Nothing -> Nothing
    Just xs -> Just $ map (\x -> read x :: Int) xs

rectRegex :: Regex
rectRegex = mkRegex "rect ([0-9]*)x([0-9]*)"

rowRegex :: Regex
rowRegex = mkRegex "rotate row y=([0-9]*) by ([0-9]*)"

columnRegex :: Regex
columnRegex = mkRegex "rotate column x=([0-9]*) by ([0-9]*)"

getInstruction :: String -> Maybe Instruction
getInstruction instr =
    let
        test = tryRegex instr
    in
        case test rectRegex of
        Nothing     -> case test rowRegex of
                       Nothing     -> case test columnRegex of
                                      Nothing     -> Nothing
                                      Just [x, y] -> Just $ Column x y
                       Just [x, y] -> Just $ Row x y
        Just [x, y] -> Just $ Rect x y

makeColumn :: X -> Y -> [CoOrd] -> [CoOrd]
makeColumn x y cells = (x, y) : cells

makeRow :: Y -> X -> [CoOrd] -> [CoOrd]
makeRow y x cells = foldr (makeColumn x) cells [0..y - 1]

addRect :: X -> Y -> On -> On
addRect x y on = foldr (makeRow y) on [0..x - 1]

shiftRow :: Y -> Int -> CoOrd -> CoOrd
shiftRow row shift (x, y)
    | y == row  = (rem (x + shift) 50, y)
    | otherwise = (x, y)

shiftColumn :: X -> Int -> CoOrd -> CoOrd
shiftColumn column shift (x, y)
    | x == column = (x, rem (y + shift) 6)
    | otherwise   = (x, y)

updateDisplay :: String -> On -> On
updateDisplay instruction on =
    case getInstruction instruction of
    Just (Rect x y)   -> addRect x y on
    Just (Row x y)    -> map (shiftRow x y) on
    Just (Column x y) -> map (shiftColumn x y) on
    Nothing           -> on

createDisplay :: On -> Int -> IO Int
createDisplay on 0         = return $ length $ nub on
createDisplay on lineCount = do
    line <- getLine
    createDisplay (updateDisplay line on) $ lineCount - 1

fillBlanks :: [(Int, Int)] -> String
fillBlanks []                 = []
fillBlanks ((a, b):(c, d):xs) =
    ['◼'] ++ (take (c - a - 1) $ repeat ' ') ++ (fillBlanks ((c, d) : xs))
fillBlanks ((x, y):[])
    | x < 49                  = ['◼'] ++ (take (49 - x) $ repeat ' ')
    | otherwise               = ['◼']

fillToText :: [(Int, Int)] -> [String] -> [String]
fillToText on@((x, y):xs) txtRows
    | x > 0     = ((take (x - 0) $ repeat ' ') ++ (fillBlanks on)) : txtRows
    | otherwise = (fillBlanks on) : txtRows

displayToText :: [[(Int, Int)]] -> [String]
displayToText = foldr fillToText []

sortByFst :: (Int, Int) -> (Int, Int) -> Ordering
sortByFst = comparing fst

sortBySnd :: (Int, Int) -> (Int, Int) -> Ordering
sortBySnd = comparing snd

sortByRowThenColumn :: (Int, Int) -> (Int, Int) -> Ordering
sortByRowThenColumn = mconcat [sortBySnd, sortByFst]

toRows :: On -> [[(Int, Int)]]
toRows = (groupBy (\x y -> snd x == snd y)) . (sortBy sortByRowThenColumn)

printLines :: [String] -> IO ()
printLines []     = return ()
printLines (x:xs) = do
    putStrLn x
    printLines xs

printDisplay :: Int -> On -> IO ()
printDisplay 0         on = printLines $ displayToText . toRows . nub $ on
printDisplay lineCount on = do
    line <- getLine
    printDisplay (lineCount - 1) $ updateDisplay line on

readAndWrite :: (Int -> IO Int) -> IO ()
readAndWrite method = do
    lineCount <- getLine
    let n = read lineCount :: Int
    result    <- method n
    putStrLn . show $ result

main1 :: IO ()
main1 = readAndWrite $ createDisplay []

main2 :: IO ()
main2 = do
    lineCount <- getLine
    let n = read lineCount :: Int
    printDisplay n []
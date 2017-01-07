import Data.List.Split (splitOn)
import Data.Ix (inRange)

data Direction =
      North
    | South
    | East
    | West

getTurnAndDistance :: String -> (Char, Int)
getTurnAndDistance (dir:distance) = (dir, read distance :: Int)

updatePosition :: (Int, Int) -> Direction -> Int -> (Int, Int)
updatePosition (x, y) North distance = (x, y + distance)
updatePosition (x, y) South distance = (x, y - distance)
updatePosition (x, y) East  distance = (x + distance, y)
updatePosition (x, y) West  distance = (x - distance, y)

updateDir :: Direction -> Char -> Direction
updateDir North 'R' = East
updateDir North 'L' = West
updateDir East  'R' = South
updateDir East  'L' = North
updateDir South 'R' = West
updateDir South 'L' = East
updateDir West  'R' = North
updateDir West  'L' = South

blocksAway :: (Int, Int) -> Direction -> [String] -> Int
blocksAway (x, y)   _         []       = sum $ map abs [x, y]
blocksAway position direction (x:xs)   =
    let
        (turn, distance) = getTurnAndDistance x
        newDir           = updateDir direction turn
        newPos           = updatePosition position newDir distance
    in
        blocksAway newPos newDir xs

type X = Int
type Y = Int
type CoOrd = (X, Y)

isBetween :: Int -> Int -> Int -> Bool
isBetween p1 p2 x = inRange (p1, p2) x || inRange (p2, p1) x

getIntersectionCoordinates :: X -> X -> X -> Y -> Y -> Y -> Maybe CoOrd
getIntersectionCoordinates x1 x2 x y1 y2 y =
    if isBetween x1 x2 x && isBetween y1 y2 y
    then Just (x, y)
    else Nothing

getIntersection :: CoOrd -> CoOrd -> (CoOrd, CoOrd) -> Maybe CoOrd
getIntersection (x1, y1) (x2, y2) ((px1, py1), (px2, py2))
    | x1 == x2 && py1 == py2 = getIntersectionCoordinates px1 px2 x1 y1 y2 py1
    | y1 == y2 && px1 == px2 = getIntersectionCoordinates x1 x2 px1 py1 py2 y1
    | otherwise              = Nothing

getIntersectionFromList :: CoOrd -> CoOrd -> [(CoOrd, CoOrd)] -> Maybe CoOrd
getIntersectionFromList start end []     = Nothing
getIntersectionFromList start end (x:xs) =
    case getIntersection start end x of
    Nothing  -> getIntersectionFromList start end xs
    Just pos -> Just pos

blocksAwayOfCross :: CoOrd -> Direction -> [(CoOrd, CoOrd)] -> [String] -> Int
blocksAwayOfCross position direction paths (x:xs)   =
    let
        (turn, distance) = getTurnAndDistance x
        newDir           = updateDir direction turn
        newPos           = updatePosition position newDir distance
    in
        case getIntersectionFromList position newPos $ drop 2 paths of
        Nothing     -> blocksAwayOfCross newPos newDir ((position, newPos):paths) xs
        Just (x, y) -> sum $ map abs [x, y]

main1 :: IO()
main1 = do
  str <- getLine
  putStrLn . show $ blocksAway (0,0) North $ splitOn ", " str

main2 :: IO()
main2 = do
  str <- getLine
  putStrLn . show $ blocksAwayOfCross (0,0) North [] $ splitOn ", " str

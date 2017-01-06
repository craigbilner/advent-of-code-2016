import Data.List.Split (splitOn)

data Direction =
      North
    | South
    | East
    | West

blocksAway :: (Int, Int) -> Direction -> [String] -> Int
blocksAway (x, y)   _         []       = sum $ map abs [x, y]
blocksAway position direction (x:xs)   =
    let
        (turn, distance) = getTurnAndDistance x
        newDir           = updateDir direction turn
        newPos           = updatePosition position newDir distance
    in
        blocksAway newPos newDir xs
    where getTurnAndDistance (dir:distance) = (dir, read distance :: Int)
          updatePosition (x, y) North distance = (x, y + distance)
          updatePosition (x, y) South distance = (x, y - distance)
          updatePosition (x, y) East  distance = (x + distance, y)
          updatePosition (x, y) West  distance = (x - distance, y)
          updateDir North 'R' = East
          updateDir North 'L' = West
          updateDir East  'R' = South
          updateDir East  'L' = North
          updateDir South 'R' = West
          updateDir South 'L' = East
          updateDir West  'R' = North
          updateDir West  'L' = South

main :: IO()
main = do
  str <- getLine
  putStrLn . show $ blocksAway (0,0) North $ splitOn ", " str
type CoOrd = (Int, Int)

type Instruction = Char

type Instructions = String

type Key = Int

keys :: [[Int]]
keys = [ [1, 2, 3]
       , [4, 5, 6]
       , [7, 8, 9]
       ]

updateKey :: CoOrd -> Instruction -> CoOrd
updateKey (x, y) 'U' = (x, min 2 $ y + 1)
updateKey (x, y) 'D' = (x, max 0 $ y - 1)
updateKey (x, y) 'L' = (max 0 $ x - 1, y)
updateKey (x, y) 'R' = (min 2 $ x + 1, y)

keyFromPos :: CoOrd -> Key
keyFromPos (x, y) = (keys !! (2 - y)) !! x

getPosition :: CoOrd -> Instructions -> CoOrd
getPosition pos []     = pos
getPosition pos (x:xs) = getPosition (updateKey pos x) xs

getPositions :: CoOrd -> [Instructions] -> [CoOrd]
getPositions _   []     = []
getPositions pos (x:xs) =
    let
        newPos = getPosition pos x
    in
        newPos : getPositions newPos xs

concatWithString :: Int -> String -> String
concatWithString x xs = show x ++ xs

writeKeys :: [Instructions] -> IO ()
writeKeys = putStrLn
            . (foldr concatWithString "")
            . (map keyFromPos)
            . (getPositions (1, 1))

main :: IO()
main = do
    inst1 <- getLine
    inst2 <- getLine
    inst3 <- getLine
    inst4 <- getLine
    inst5 <- getLine
    writeKeys [inst1, inst2, inst3, inst4, inst5]

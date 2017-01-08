toInt :: String -> Int
toInt x = read x :: Int

isTriangle :: [Int] -> Bool
isTriangle [x, y, z] = x + y > z && x + z > y && y + z > x

countTriangles :: Int -> IO Int
countTriangles 0         = return 0
countTriangles lineCount = do
    line          <- getLine
    triangleCount <- countTriangles (lineCount - 1)
    let sides = map toInt $ words line
    let count = if isTriangle sides then 1 + triangleCount else triangleCount
    return count

main :: IO ()
main = do
    lineCount    <- getLine
    let n = read lineCount :: Int
    triangleCount <- countTriangles n
    putStrLn . show $ triangleCount

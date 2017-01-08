toInt :: String -> Int
toInt x = read x :: Int

isTriangle :: [Int] -> Bool
isTriangle [x, y, z] = x + y > z && x + z > y && y + z > x

countTrianglesByRow :: Int -> IO Int
countTrianglesByRow 0         = return 0
countTrianglesByRow lineCount = do
    line          <- getLine
    triangleCount <- countTrianglesByRow (lineCount - 1)
    let sides = map toInt $ words line
    let count = if isTriangle sides then 1 + triangleCount else triangleCount
    return count

countTrianglesByColumn :: Int -> IO Int
countTrianglesByColumn 0         = return 0
countTrianglesByColumn lineCount = do
    line1          <- getLine
    line2          <- getLine
    line3          <- getLine
    triangleCount <- countTrianglesByColumn (lineCount - 3)
    let [x1, x2, x3] = map toInt $ words line1
    let [y1, y2, y3] = map toInt $ words line2
    let [z1, z2, z3] = map toInt $ words line3
    let areTriangles = [ isTriangle [x1, y1, z1]
                       , isTriangle [x2, y2, z2]
                       , isTriangle [x3, y3, z3]
                       ]
    let count = length $ filter id areTriangles
    return (count + triangleCount)

countTriangles :: (Int -> IO Int) -> IO ()
countTriangles method = do
    lineCount     <- getLine
    let n = read lineCount :: Int
    triangleCount <- method n
    putStrLn . show $ triangleCount

main1 :: IO ()
main1 = countTriangles countTrianglesByRow

main2 :: IO ()
main2 = countTriangles countTrianglesByColumn

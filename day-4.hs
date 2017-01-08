import Data.List (sort, sortBy, group, intercalate)
import Data.List.Split (splitOn)
import Data.Ord (comparing)
import Data.Char (ord, chr)

sortByLengthDesc :: String -> String -> Ordering
sortByLengthDesc = flip $ comparing length

sortByAlpha :: String -> String -> Ordering
sortByAlpha = comparing id

sortByMostFrequent :: String -> String -> Ordering
sortByMostFrequent = mconcat [sortByLengthDesc, sortByAlpha]

concatFirst :: String -> String -> String
concatFirst letters firstLetters = (head letters) : firstLetters

top5Letters :: String -> String
top5Letters =
    (foldr concatFirst "")
    . (take 5)
    . (sortBy sortByMostFrequent)
    . group
    . sort

sumSectorIds :: Int -> IO Int
sumSectorIds 0         = return 0
sumSectorIds lineCount = do
    line  <- getLine
    idSum <- sumSectorIds (lineCount - 1)
    let letterSplit = splitOn "-" line
    let letters = foldr (++) "" $ init letterSplit
    let idCheck = splitOn "[" $ last letterSplit
    let sectorId = read (head idCheck) :: Int
    let check = init $ last idCheck
    let count = if top5Letters letters == check then sectorId + idSum else idSum
    return count

rotateLetter :: Int -> Char -> Char
rotateLetter _ '-'   = ' '
rotateLetter turns l = chr $ (rem (ord l - 97 + turns) 26) + 97

decipher :: Int -> String -> String
decipher _     []     = []
decipher turns (x:xs) = (rotateLetter turns x) : decipher turns xs

decipherCodes :: Int -> IO [(Int, String)]
decipherCodes 0         = return []
decipherCodes lineCount = do
    line  <- getLine
    codes <- decipherCodes (lineCount - 1)
    let letterSplit = splitOn "-" line
    let idCheck = splitOn "[" $ last letterSplit
    let sectorId = read (head idCheck) :: Int
    let deciphered = decipher sectorId $ intercalate "-" $ init letterSplit
    return ((sectorId, deciphered) : codes)

readAndWrite :: Show a => (Int -> IO a) -> IO ()
readAndWrite method = do
    lineCount <- getLine
    let n = read lineCount :: Int
    result    <- method n
    putStrLn . show $ result

main1 :: IO ()
main1 = readAndWrite sumSectorIds

main2 :: IO ()
main2 = readAndWrite decipherCodes
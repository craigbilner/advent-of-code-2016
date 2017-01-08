import Data.List (sort, sortBy, group)
import Data.List.Split (splitOn)
import Data.Ord (comparing)

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

main :: IO ()
main = do
    lineCount <- getLine
    let n = read lineCount :: Int
    idSum     <- sumSectorIds n
    putStrLn . show $ idSum
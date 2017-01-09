import qualified Data.Map as M
import Data.List (sort, sortBy, group)
import Data.Ord (comparing)

updateMap :: (Int, Char) -> M.Map Int String -> M.Map Int String
updateMap (indx, c) m =
    M.insertWith (++) indx [c] m

sortByLengthDesc :: String -> String -> Ordering
sortByLengthDesc = flip $ comparing length

sortByLetterFrequency :: String -> [String]
sortByLetterFrequency = (sortBy sortByLengthDesc) . group . sort

getMostFrequentLetter :: String -> String -> String
getMostFrequentLetter letters mf =
    let
        letter = head . head . sortByLetterFrequency $ letters
    in
        letter : mf

getLeastFrequentLetter :: String -> String -> String
getLeastFrequentLetter letters mf =
    let
        letter = head . last . sortByLetterFrequency $ letters
    in
        letter : mf

getFrequentLetter :: (String -> String -> String) -> M.Map Int String -> Int -> IO String
getFrequentLetter f m 0         = return $ M.fold f "" m
getFrequentLetter f m lineCount = do
    line  <- getLine
    let updatedMap = foldr updateMap m $ zip [1..] line
    getFrequentLetter f updatedMap (lineCount - 1)

getMostFrequent :: M.Map Int String -> Int -> IO String
getMostFrequent = getFrequentLetter getMostFrequentLetter

getLeastFrequent :: M.Map Int String -> Int -> IO String
getLeastFrequent = getFrequentLetter getLeastFrequentLetter

readAndWrite :: Show a => (Int -> IO a) -> IO ()
readAndWrite method = do
    lineCount <- getLine
    let n = read lineCount :: Int
    result    <- method n
    putStrLn . show $ result

main1 :: IO ()
main1 = readAndWrite $ getMostFrequent M.empty

main2 :: IO ()
main2 = readAndWrite $ getLeastFrequent M.empty

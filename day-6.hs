import qualified Data.Map as M
import Data.List (sort, sortBy, group)
import Data.Ord (comparing)

updateMap :: (Int, Char) -> M.Map Int String -> M.Map Int String
updateMap (indx, c) m =
    M.insertWith (++) indx [c] m

sortByLengthDesc :: String -> String -> Ordering
sortByLengthDesc = flip $ comparing length

getMostFrequentLetter :: String -> String -> String
getMostFrequentLetter letters mf =
    let
        letter = head . head . (sortBy sortByLengthDesc) . group . sort $ letters
    in
        letter : mf

getMostFrequent :: M.Map Int String -> Int -> IO String
getMostFrequent m 0         = return $ M.fold getMostFrequentLetter "" m
getMostFrequent m lineCount = do
    line  <- getLine
    let updatedMap = foldr updateMap m $ zip [1..] line
    getMostFrequent updatedMap (lineCount - 1)

readAndWrite :: Show a => (Int -> IO a) -> IO ()
readAndWrite method = do
    lineCount <- getLine
    let n = read lineCount :: Int
    result    <- method n
    putStrLn . show $ result

main1 :: IO ()
main1 = readAndWrite $ getMostFrequent M.empty

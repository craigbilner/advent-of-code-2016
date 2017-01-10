import Text.Regex.Posix
import Data.List (any)
import Data.List.Split (chunksOf)

type IP = String

isABBA :: String -> Bool
isABBA [a, b, c, d] = a /= b && [a, b] == [d, c]
isABBA _            = False

matchBrackets :: String -> [String]
matchBrackets s = getAllTextMatches $ s =~ "\\[([a-z]*)]"

bracketHasABBA :: String -> Bool
bracketHasABBA ip =
    any containsABBA $ matchBrackets ip

containsABBA :: String -> Bool
containsABBA (a:b:c:d:xs) = if isABBA [a, b, c, d] then True else containsABBA $ b : c : d : xs
containsABBA _            = False

supportsTLS :: IP -> Bool
supportsTLS ip =
    all (\f -> f ip) [not . bracketHasABBA, containsABBA]

countTLSSupport :: Int -> IO Int
countTLSSupport 0         = return 0
countTLSSupport lineCount = do
    line  <- getLine
    count <- countTLSSupport (lineCount - 1)
    return $ (+) count $ if supportsTLS line then 1 else 0

readAndWrite :: Show a => (Int -> IO a) -> IO ()
readAndWrite method = do
    lineCount <- getLine
    let n = read lineCount :: Int
    result    <- method n
    putStrLn . show $ result

main1 :: IO ()
main1 = readAndWrite $ countTLSSupport
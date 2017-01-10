import Text.Regex.Posix
import Data.List (any, isInfixOf)
import Data.List.Split (chunksOf)
import Text.Regex (mkRegex, subRegex)

type IP = String

type BAB = String

regexStr :: String
regexStr = "\\[([a-z]*)]"

isABBA :: String -> Bool
isABBA [a, b, c, d] = a /= b && [a, b] == [d, c]
isABBA _            = False

matchBrackets :: String -> [String]
matchBrackets s = getAllTextMatches $ s =~ regexStr

bracketHasABBA :: String -> Bool
bracketHasABBA ip =
    any containsABBA $ matchBrackets ip

containsABBA :: String -> Bool
containsABBA (a:b:c:d:xs) =
    if isABBA [a, b, c, d]
    then True
    else containsABBA $ b : c : d : xs
containsABBA _            = False

supportsTLS :: IP -> Bool
supportsTLS ip =
    all (\f -> f ip) [not . bracketHasABBA, containsABBA]

getBAB :: String -> [BAB] -> [BAB]
getBAB (a:b:c:xs) matches =
    let
        f = getBAB (b:c:xs)
    in
        if a == c && a /= b
        then f ([a, b, c] : matches)
        else f matches
getBAB _          matches = matches

hasMatchingABA :: BAB -> IP -> Bool
hasMatchingABA (b:a:_) = isInfixOf [a,b,a]

hasABA :: IP -> [BAB] -> Bool
hasABA = any . (flip hasMatchingABA)

supportsSSL :: IP -> Bool
supportsSSL ip =
    hasABA (subRegex (mkRegex regexStr) ip "") $ foldr getBAB [] $ matchBrackets ip

countTLSSupport :: Int -> IO Int
countTLSSupport 0         = return 0
countTLSSupport lineCount = do
    line  <- getLine
    count <- countTLSSupport (lineCount - 1)
    return $ (+) count $ if supportsTLS line then 1 else 0

countSSLSupport :: Int -> IO Int
countSSLSupport 0         = return 0
countSSLSupport lineCount = do
    line  <- getLine
    count <- countSSLSupport (lineCount - 1)
    return $ (+) count $ if supportsSSL line then 1 else 0

readAndWrite :: Show a => (Int -> IO a) -> IO ()
readAndWrite method = do
    lineCount <- getLine
    let n = read lineCount :: Int
    result    <- method n
    putStrLn . show $ result

main1 :: IO ()
main1 = readAndWrite $ countTLSSupport

main2 :: IO ()
main2 = readAndWrite $ countSSLSupport
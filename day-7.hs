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

countSupport :: (IP -> Bool) -> Int -> IO Int
countSupport _ 0         = return 0
countSupport f lineCount = do
    line  <- getLine
    count <- countSupport f (lineCount - 1)
    return $ (+) count $ if f line then 1 else 0

readAndWrite :: (IP -> Bool) -> IO ()
readAndWrite method = do
    lineCount <- getLine
    let n = read lineCount :: Int
    result    <- countSupport method n
    putStrLn . show $ result

main1 :: IO ()
main1 = readAndWrite $ supportsTLS

main2 :: IO ()
main2 = readAndWrite $ supportsSSL
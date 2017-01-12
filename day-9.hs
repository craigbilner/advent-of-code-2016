import Text.Regex (mkRegex, matchRegex, Regex)

spanWhile :: (a -> Bool) -> [a] -> ([a], [a])
spanWhile f xs = go f [] xs
    where go _ _       []     = (xs, [])
          go f matched (x:xs)
            | f x       = go f (x : matched) xs
            | otherwise =
                if length matched == 0
                then ([], x : xs)
                else (reverse (x : matched), xs)

markerRegex :: Regex
markerRegex = mkRegex "\\(([0-9]*)x([0-9]*)\\)"

getMarker :: String -> (Int, Int)
getMarker str =
    case matchRegex markerRegex str of
    Just [a, b] -> (read a :: Int, read b :: Int)
    _           -> (0, 0)

decompress :: String -> String
decompress []   = []
decompress t@(x:xs)
    | x /= '('  = x : decompress xs
    | otherwise =
        let
            markerStr              = spanWhile (/=')') t
            (takeChar, repetition) = getMarker $ fst markerStr
            duped                  = take repetition $ repeat $ take takeChar $ snd markerStr
        in
            (concat duped) ++ (decompress $ drop takeChar $ snd markerStr)

main :: IO ()
main = do
    str <- getLine
    putStrLn . show . length $ decompress str

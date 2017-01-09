import qualified Data.ByteString.Lazy.Char8 as C
import Data.Digest.Pure.MD5 (md5)

type DoorID = String

type Indx = Int

type Password = String

stringToMD5Hex :: String -> String
stringToMD5Hex = show . md5 . C.pack

iterateIndx :: DoorID -> Indx -> Password -> Password
iterateIndx doorID indx password
    | length password == 8 = password
    | otherwise            =
        let
            hash = stringToMD5Hex $ doorID ++ show indx
            next = iterateIndx doorID $ indx + 1
        in
            if take 5 hash == "00000"
            then next $ (hash !! 5) : password
            else next password

getPassword :: DoorID -> Password
getPassword doorID = reverse $ iterateIndx doorID 0 ""

main :: IO ()
main = do
    doorID <- getLine
    putStrLn . getPassword $ doorID
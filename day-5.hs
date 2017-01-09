import qualified Data.ByteString.Lazy.Char8 as C
import Data.Digest.Pure.MD5 (md5)
import qualified Data.Map as Map
import Data.Char (ord)

type DoorID = String

type Indx = Int

type Password = String

type Position = Char

type Key = Char

type PasswordPositions = Map.Map Position Key

type Hash = String

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

positionsToPassword :: PasswordPositions -> Password
positionsToPassword = Map.fold (:) ""

positionIsValid :: Position -> Bool
positionIsValid p =
    let
        pOrd = ord p
    in
        pOrd >= 48 && pOrd <= 55

positionExists :: Position -> PasswordPositions -> Bool
positionExists p pp =
    case Map.lookup p pp of
    Nothing -> False
    _       -> True

updatePositions :: Hash -> PasswordPositions -> PasswordPositions
updatePositions hash pp
    | take 5 hash /= "00000" = pp
    | otherwise              =
        let
            position  = hash !! 5
            character = hash !! 6
        in
            if positionIsValid position && not (positionExists position pp)
            then Map.insert position character pp
            else pp

iterateIndxWithPosition :: DoorID -> Indx -> PasswordPositions -> Password
iterateIndxWithPosition doorID indx pp
    | Map.size pp == 8 = positionsToPassword pp
    | otherwise        =
        let
            hash = stringToMD5Hex $ doorID ++ show indx
            next = iterateIndxWithPosition doorID $ indx + 1
        in
            next $ updatePositions hash pp

main1 :: IO ()
main1 = do
    doorID <- getLine
    putStrLn $ reverse $ iterateIndx doorID 0 ""

main2 :: IO ()
main2 = do
    doorID <- getLine
    putStrLn $ iterateIndxWithPosition doorID 0 Map.empty
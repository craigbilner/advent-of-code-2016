type CoOrd = (Int, Int)

type Instruction = Char

type Instructions = String

type Key = Char

type KeyPad = [[Char]]

keyPad1 :: KeyPad
keyPad1 = [ ['0', '0', '0', '0', '0']
          , ['0', '1', '2', '3', '0']
          , ['0', '4', '5', '6', '0']
          , ['0', '7', '8', '9', '0']
          , ['0', '0', '0', '0', '0']
          ]

keyPad2 :: KeyPad
keyPad2 = [ ['0', '0', '0', '0', '0', '0', '0']
          , ['0', '0', '0', '1', '0', '0', '0']
          , ['0', '0', '2', '3', '4', '0', '0']
          , ['0', '5', '6', '7', '8', '9', '0']
          , ['0', '0', 'A', 'B', 'C', '0', '0']
          , ['0', '0', '0', 'D', '0', '0', '0']
          , ['0', '0', '0', '0', '0', '0', '0']
          ]

tryUpdate :: KeyPad -> CoOrd -> CoOrd -> CoOrd
tryUpdate keyPad defaultPos pos =
    if keyFromPos keyPad pos == '0'
    then defaultPos
    else pos

updateKey :: KeyPad -> CoOrd -> Instruction -> CoOrd
updateKey keyPad pos instruction = tryUpdate keyPad pos $ go pos instruction
    where go (x, y) 'U' = (x, y + 1)
          go (x, y) 'D' = (x, y - 1)
          go (x, y) 'L' = (x - 1, y)
          go (x, y) 'R' = (x + 1, y)

keyFromPos :: KeyPad -> CoOrd -> Key
keyFromPos keyPad (x, y) = (keyPad !! ((length keyPad - 1) - y)) !! x

getPosition :: KeyPad -> CoOrd -> Instructions -> CoOrd
getPosition _      pos []     = pos
getPosition keyPad pos (x:xs) = getPosition keyPad (updateKey keyPad pos x) xs

getPositions :: KeyPad -> CoOrd -> [Instructions] -> [CoOrd]
getPositions _      _   []     = []
getPositions keyPad pos (x:xs) =
    let
        newPos = getPosition keyPad pos x
    in
        newPos : getPositions keyPad newPos xs

writeKeys :: KeyPad -> CoOrd -> [Instructions] -> IO ()
writeKeys keyPad pos =
    putStrLn
    . (map (keyFromPos keyPad))
    . (getPositions keyPad pos)

getAndWrite :: KeyPad -> CoOrd -> IO ()
getAndWrite keyPad pos = do
    inst1 <- getLine
    inst2 <- getLine
    inst3 <- getLine
    inst4 <- getLine
    inst5 <- getLine
    writeKeys keyPad pos [inst1, inst2, inst3, inst4, inst5]

main1 :: IO ()
main1 = getAndWrite keyPad1 (2, 2)

main2 :: IO ()
main2 = getAndWrite keyPad2 (1, 3)

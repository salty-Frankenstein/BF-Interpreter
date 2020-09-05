import Control.Monad.State
import Data.Char
import Zipper
import Brackets

type Byte = Int

succB, predB :: Byte -> Byte
succB x = if x == 255 then 0 else x+1
predB x = if x == 0 then 255 else x-1

type Tape = Zipper Byte
nextByte, prevByte, succByte, predByte, printByte, getByte :: StateT Tape IO ()
nextByte = StateT $ \z -> return ((), next z)
prevByte = StateT $ \z -> return ((), prev z)
succByte = StateT $ \z -> return ((), mutZip succB z)
predByte = StateT $ \z -> return ((), mutZip predB z)
printByte = do 
    b <- StateT $ \z -> return (current z, z)
    liftIO $ putChar $ chr b

getByte = do
    c <- liftIO $ getChar
    StateT $ \z -> return ((), mutZip (\_ -> ord c) z)

currentByte :: StateT Tape IO Byte
currentByte = StateT $ \z -> return (current z, z)

type Code = Zipper Char
nextIns, prevIns :: StateT Code (StateT Tape IO) ()
nextIns = StateT $ \z -> return ((), next z)
prevIns = StateT $ \z -> return ((), prev z)

getIns :: StateT Code (StateT Tape IO) Char
getIns = StateT $ \z -> return (current z, z)

isBegin, isEnd :: StateT Code (StateT Tape IO) Bool
isBegin = StateT $ \z -> return (isHead z, z)
isEnd = StateT $ \z -> return (isTail z, z)

getPosition :: StateT Code (StateT Tape IO) Int
getPosition = StateT $ \z -> return (position z, z)

-- jump to a position
findPosition :: Int -> StateT Code (StateT Tape IO) ()
findPosition x = do
    p <- getPosition
    if x > p then do
        nextIns
        findPosition x
    else if x < p then do
        prevIns
        findPosition x
    else return ()

findMatch :: [(Int, Int)] -> Int -> Int
findMatch l x = 
    let [(a,b)] = filter (\(a,b) -> a==x || b==x) l in
        if a==x then b else a


interpret :: [(Int, Int)] -> StateT Code (StateT Tape IO) ()
interpret table = do
    f <- isEnd
    if f then return ()
    else do
        c <- getIns
        case c of
            '>' -> lift nextByte
            '<' -> lift prevByte
            '+' -> lift succByte
            '-' -> lift predByte
            '.' -> lift printByte
            ',' -> lift getByte
            '[' -> do
                v <- lift currentByte
                if v == 0 then do
                    p <- getPosition
                    findPosition $ findMatch table p
                else return ()
            ']' -> do
                v <- lift currentByte
                if v /= 0 then do
                    p <- getPosition
                    findPosition $ findMatch table p
                else return ()
            _ -> error "syntax error"
        nextIns
        interpret table

isIns :: Char -> Bool
isIns c = case c of
    '>' -> True
    '<' -> True
    '+' -> True
    '-' -> True
    '.' -> True
    ',' -> True
    '[' -> True
    ']' -> True
    _ -> False

-- with an ending signal
toCode :: String -> Code
toCode s = fromList $ filter isIns s ++ "."

stream :: Int -> [Int]
stream a = a : stream a

main :: IO ()
main = do
    code <- getLine
    (runStateT $ runStateT (interpret $ runMatch code ('[', ']')) $ toCode code) $ fromList $ stream 0
    return ()

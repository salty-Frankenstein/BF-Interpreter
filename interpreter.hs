import Control.Monad.State
import Data.Char
import Zipper

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

evalSeq :: StateT Tape IO ()
evalSeq = do
    getByte
    printByte
    nextByte
    getByte
    printByte
    prevByte
    succByte
    printByte

type Code = Zipper Char
nextIns, prevIns :: StateT Code (StateT Tape IO) ()
nextIns = StateT $ \z -> return ((), next z)
prevIns = StateT $ \z -> return ((), prev z)

getIns :: StateT Code (StateT Tape IO) Char
getIns = StateT $ \z -> return (current z, z)

isBegin, isEnd :: StateT Code (StateT Tape IO) Bool
isBegin = StateT $ \z -> return (isHead z, z)
isEnd = StateT $ \z -> return (isTail z, z)



interpret :: StateT Code (StateT Tape IO) ()
interpret = do
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
            '[' -> return ()
            ']' -> return ()
            _ -> error "syntax error"
        nextIns
        interpret
        return ()


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

toCode :: String -> Code
toCode s = fromList $ filter isIns s ++ "."

stream :: Int -> [Int]
stream a = a : stream a

main :: IO ()
main = do
    --runStateT evalSeq $ fromList $ stream 0
    (runStateT $ runStateT interpret $ toCode ((replicate 65 '+')++".>,.<.")) $ fromList $ stream 0
    putStr "\n"
    return ()

import Control.Monad.State
import Data.Char
import Zipper

type Byte = Int

succB, predB :: Byte -> Byte
succB x = if x == 255 then 0 else x+1
predB x = if x == 0 then 255 else x-1

data Tape = Tape { tape::(Zipper Byte) }
--instance Show Tape where
--    show (Tape x) = show x

nextByte, prevByte, succByte, predByte, printByte, getByte :: StateT Tape IO ()
nextByte = StateT $ \(Tape z) -> return ((), Tape $ next z)
prevByte = StateT $ \(Tape z) -> return ((), Tape $ prev z)
succByte = StateT $ \(Tape z) -> return ((), Tape $ mutZip succB z)
predByte = StateT $ \(Tape z) -> return ((), Tape $ mutZip predB z)
printByte = do 
    b <- StateT $ \(Tape z) -> return (current z, Tape z)
    liftIO $ putChar $ chr b

getByte = do
    c <- liftIO $ getChar
    StateT $ \(Tape z) -> return ((), Tape $ mutZip (\_ -> ord c) z)

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

stream :: Int -> [Int]
stream a = a : stream a

main :: IO ()
main = do 
    runStateT evalSeq $ Tape $ fromList $ stream 0
    return ()

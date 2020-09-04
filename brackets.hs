module Brackets where

import Control.Monad.State
import Control.Monad.Writer

write :: (Int, Int) -> Writer [(Int, Int)] ()
write p = writer ((), [p])

type Stack = [Int]

push :: Int -> StateT Stack (Writer [(Int, Int)]) ()
push x = StateT $ \s -> return ((), x:s)

pop :: StateT Stack (Writer [(Int, Int)]) Int
pop = StateT $ \(s:ss) -> return (s, ss)

isEmpty :: StateT Stack (Writer [(Int, Int)]) Bool
isEmpty = StateT $ \s -> return (s==[], s)

match :: String -> (Char, Char) -> Int -> StateT Stack (Writer [(Int, Int)]) ()
match [] _ _ = return ()
match (s:ss) (l, r) i = do
    if s == l then 
        push i
    else if s == r then do
        f <- isEmpty
        if f then error "not matched" 
        else do
            v <- pop
            lift $ write (v, i)
    else return ()
    match ss (l, r) (i+1)

runMatch :: String -> (Char, Char) -> [(Int, Int)]
runMatch s p = snd$ runWriter $ (runStateT $ match s p 0) []



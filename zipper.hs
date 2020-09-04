module Zipper where

data Zipper a = Zipper [a] a [a] deriving Show
fromList :: [a] -> Zipper a
fromList (x:xs) = Zipper [] x xs
fromList _ = error "empty!"

current :: Zipper a -> a
current (Zipper h x t) = x

next :: Zipper a -> Zipper a
next (Zipper ys y (x:xs)) = Zipper (y : ys) x xs
next z = z

prev :: Zipper a -> Zipper a
prev (Zipper (y:ys) x xs) = Zipper ys y (x : xs)
prev z = z

isHead, isTail :: Zipper a -> Bool
isHead (Zipper [] _ _) = True
isHead _ = False
isTail (Zipper _ _ []) = True
isTail _ = False 

mutZip :: (a -> a) -> Zipper a -> Zipper a
mutZip f (Zipper h x t) = Zipper h (f x) t

position :: Zipper a -> Int
position (Zipper h _ _) = length h

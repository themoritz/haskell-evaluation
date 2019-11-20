{-# LANGUAGE BangPatterns #-}

module Evaluation where

import Prelude hiding (foldr, foldl, foldl', not)

import Control.Exception (evaluate)
import Debug.Trace (trace)

data Foo a = Foo a

caf :: Foo (Foo Int)
caf = Foo (Foo 1)

true :: Bool
true = True

f :: Int -> Foo Int
f !i =
  trace "f" (Foo i)

g :: Int -> Int
g i =
  trace "g" (i + 1)

-- >>> _ <- evaluate caf
-- >>> :sprint caf
-- caf = Foo _

-- >>> let x = let y = g 1 in [f y, Foo y]
-- >>> _ <- evaluate (head x)
-- g
-- f
-- >>> :sprint x
-- x = [Foo 2,Foo 2]

-- >>> let x = not true
-- >>> _ <- evaluate x
-- >>> :sprint x
-- x = False

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z []     = z
foldr f z (x:xs) = x `f` foldr f z xs

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f z []     = z
foldl f z (x:xs) =
  let
    z' = z `f` x
  in
    foldl f z' xs

{-# LANGUAGE BangPatterns #-}

module Evaluation where

import Prelude hiding (foldl')

import Control.Exception (evaluate)
import Debug.Trace (trace)

-- >>> :set -XBangPatterns

f :: Int -> Maybe Int
f i =
  case i of
    1 -> Just 1
    i -> Just i


g :: Int -> Maybe Int
g !i = Just i

data SMaybe a =
    SJust !a
  | SNothing

s :: Int -> SMaybe Int
s i = SJust i

-- >>> let !x = s (1 + 1)
-- >>> :sprint x
-- x = SJust 2

class NFData a where
  rnf :: a -> ()

instance NFData a => NFData (SMaybe a) where
  rnf (SJust a) = rnf a `seq` ()

data Pair = Pair !Int !Int

wrap :: Int -> Pair
wrap i =
  let
    x = trace "computing x" (i + 1)
    y = trace "computing y" (i + 2)
  in
    Pair x y

-- >>> let !x = wrap 1
-- computing x
-- computing y
-- >>> :sprint x
-- x = Pair 2 3

data SList a =
    SCons !a !(SList a)
  | SNil


data Spine a = Spine ![a]

smap :: (a -> b) -> Spine a -> Spine b
smap f (Spine xs) = Spine (loop xs)
  where
    loop []     = []
    loop (x:xs) =
      let
        !fx = f x
        !fxs = loop xs
      in
        fx : fxs

foo :: Bool -> Maybe Bool
foo b = Just (not b)

-- >>> let !x = smap foo (Spine [True, False, True])
-- >>> :sprint x
-- x = Spine [Just _,Just _,Just _]


foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f z []     = z
foldl' f z (x:xs) =
  case z `f` x of
    !z' -> foldl' f z' xs

-- >>> let !x = foldl' (+) 0 [1..10000000 :: Int]
-- >>> :sprint x
-- x =

mean :: [Double] -> Double
mean xs = sum / count
  where
    (sum, count) = foldl' go (0, 0) xs
    go (!sum, !count) x = (sum + x, count + 1)

-- >>> mean [1,2,3]
-- 2.0

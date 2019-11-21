{-# LANGUAGE BangPatterns #-}

module Evaluation where

import Prelude hiding (foldl, foldl')

import Control.Exception (evaluate)
import Debug.Trace (trace)

-- >>> :set -XBangPatterns

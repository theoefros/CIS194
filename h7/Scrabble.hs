{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Scrabble where

import Data.Monoid
import Data.Char (toLower)

newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

getScore :: Score -> Int
getScore (Score s) = s

instance Monoid Score where
  mempty = 0
  mappend = (+)
  
score :: Char -> Score
score c = case lookup (toLower c) scoreLUT of
            Just s  -> Score s
            Nothing -> Score 0

scoreString :: String -> Score
scoreString = mconcat . map score

scoreLUT :: [(Char, Int)]
scoreLUT = [
  ('a', 1)
  ,('b', 3)
  ,('c', 3)
  ,('d', 2)
  ,('e', 1)
  ,('f', 4)
  ,('g', 2)
  ,('h', 4)
  ,('i', 1)
  ,('j', 8)
  ,('k', 5)
  ,('l', 1)
  ,('m', 3)
  ,('n', 1)
  ,('o', 1)
  ,('p', 3)
  ,('q', 10)
  ,('r', 1)
  ,('s', 1)
  ,('t', 1)
  ,('u', 1)
  ,('v', 4)
  ,('w', 4)
  ,('x', 8)
  ,('y', 4)
  ,('z', 10)]
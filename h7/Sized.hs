{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Sized where

import Data.Monoid

-- | Size is simply a newtype wrapper around Int
newtype Size = Size Int
  deriving (Eq, Ord, Show, Num)

getSize :: Size -> Int
getSize (Size i) = i

-- | A type class to provide a method for obtaining a size of a value
class Sized a where
  size :: a -> Size

instance Sized Size where
  size = id

-- This instance means that things like
--   (Foo, Size)
--   (Foo, (Bar, Size))
--   ...
-- are all instances of Sized.
instance Sized b => Sized (a,b) where
  size = size . snd

-- Size is an instance of monoid, under addition
instance Monoid Size where
  mempty  = Size 0
  mappend = (+)

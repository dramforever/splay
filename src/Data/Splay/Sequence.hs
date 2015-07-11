{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Splay.Sequence
-- Copyright   :  (c) dramforever 2015
-- License     :  BSD3
-- Maintainer  :  dramforever
-- Stability   :  unstable
-- Portability :  non-portable (GHC extensions)
--
-- An example implementation of sequences using splay trees.
--
-- Some of the names in this module clashes with "Prelude" functions, so
-- this module should be imported qualified. 
-----------------------------------------------------------------------------

module Data.Splay.Sequence
       ( Seq
       , singleton
       , (<|)
       , (|>)
       , fromList
       , null
       , length
       , splitAt
       ) where

import Control.Applicative
import Data.Foldable hiding (null, length)
import Data.Function
import Data.Monoid
import Data.Traversable
import Prelude hiding (null, length, splitAt, foldr)

import qualified Data.Splay as S

newtype Size = Size { getSize :: Int }
               deriving (Num, Eq, Ord)

newtype Item a = Item { getItem :: a }

instance Monoid Size where
  mempty = 0
  mappend = (+)

instance S.Measured Size (Item a) where
  measure _ = 1

-- | General purpose finite sequences
newtype Seq a = Seq { getSeq :: S.Splay Size (Item a) }

instance Functor Seq where fmap = fmapDefault
instance Foldable Seq where foldMap = foldMapDefault

instance Traversable Seq where
  traverse f (Seq x) = Seq <$> S.traverseSplay (fmap Item . f . getItem) x

instance Monoid (Seq a) where
  mempty = Seq mempty
  Seq a `mappend` Seq b = Seq $ a <> b

instance Eq a => Eq (Seq a) where
  (==) = (==) `on` toList
  (/=) = (/=) `on` toList

instance Ord a => Ord (Seq a) where
  compare = compare `on` toList

instance Show a => Show (Seq a) where
  showsPrec p xs =
    showParen (p > 10) $ showString "fromList " . shows (toList xs)

-- | Construct a sequence of only one element
singleton :: a -> Seq a
singleton = Seq . S.singleton . Item

-- | Add an element to the left end of a sequence
(<|) :: a -> Seq a -> Seq a
x <| xs = singleton x <> xs

-- | Add an element to the right end of a sequence
(|>) :: Seq a -> a -> Seq a
xs |> x = xs <> singleton x

-- | Construct a sequence from a list
--
--   /Warning/: The underlying splay tree created this function will be
--   very unbalanced. However note that the amortized running time of
--   other functions will be unaffected. This is to be fixed in the future.
fromList :: [a] -> Seq a
fromList = foldr (<|) mempty

-- | Is this sequence empty?
null :: Seq a -> Bool
null = (== 0) . length

-- | The number of elements in the sequence
length :: Seq a -> Int
length = getSize . S.measure . getSeq

-- | @'splitAt' n s@ splits @s@ into the first @n@ elements and the rest.
--   If @n@ is less than the length of @s@, @(s, mempty)@ will be returned
splitAt :: Int -> Seq a -> (Seq a, Seq a)
splitAt n (Seq s) = case S.split (> Size n) s of (a, b) -> (Seq a, Seq b)

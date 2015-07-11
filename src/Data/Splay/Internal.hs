-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Splay.Internal
-- Copyright   :  (c) dramforever 2015
-- License     :  BSD3
-- Maintainer  :  dramforever
-- Stability   :  unstable
-- Portability :  non-portable (GHC extensions)
--
-- Internal module containing the definition of the @'Splay'@ type.
-----------------------------------------------------------------------------

module Data.Splay.Internal
    ( Splay(..) ) where

-- | A representation of a sequence of values of type @a@ using a splay
--   tree, which can also contain measurements of type @s@.
data Splay s a
  = Leaf
  | Branch !s a (Splay s a) (Splay s a)
    -- ^ Invariant: @'s'@ is a valid cached measurement

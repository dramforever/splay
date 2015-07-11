module Data.Splay.Internal
    ( Splay(..) ) where

-- | A representation of a sequence of values of type @a@ using a splay
--   tree, which can also contain measurements of type @s@.
data Splay s a
  = Leaf
  | Branch !s a (Splay s a) (Splay s a)

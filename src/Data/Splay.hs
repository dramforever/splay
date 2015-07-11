{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Splay
-- Copyright   :  (c) dramforever 2015
-- License     :  BSD3
-- Maintainer  :  dramforever
-- Stability   :  unstable
-- Portability :  non-portable (GHC extensions)
--
-- A generic sequence representation with arbitary annotations, for use
-- as a base for implementations of various collection types
--
-- The empty sequence and the concatenation operation are available in
-- the @'Monoid'@ instance of @'Splay'@
-----------------------------------------------------------------------------

module Data.Splay
       ( Splay
       , Measured(..)
       , singleton
       , split
       , traverseSplay
       ) where

import Control.Applicative
import Data.Monoid ((<>))
import Prelude

-- | A representation of a sequence of values of type @a@ using a splay
--   tree, which can also contain measurements of type @s@.
data Splay s a
  = Leaf
  | Branch !s a (Splay s a) (Splay s a)
    -- ^ Invariant: @'s'@ is a valid cached measurement

-- | @s@ is a measurement of @a@
class Monoid s => Measured s a | a -> s where
  measure :: a -> s

-- | This instance uses the cached measure if possible
instance Measured s a => Measured s (Splay s a) where
  measure Leaf = mempty
  measure (Branch s _ _ _) = s

-- | @'mappend'@ concatenates two sequences, and @'mempty'@ is the empty
--   sequence.
instance Measured s a => Monoid (Splay s a) where
  Leaf `mappend` b = b
  a `mappend` Leaf = a
  a `mappend` b = case splayRightmost a of
    Branch _ x tl Leaf -> branch x tl b
    _ -> error "splay: internal error"

  mempty = Leaf

splayRightmost :: Measured s a => Splay s a -> Splay s a
splayRightmost t = go id t
  where go lf Leaf = lf Leaf
        go lf (Branch _ x tl Leaf) = branch x (lf tl) Leaf
        go lf (Branch _ x tl (Branch _ xr trl Leaf)) =
          branch xr (lf $ branch x tl trl) Leaf
        go lf (Branch _ x tl (Branch _ xr trl trr)) =
          go (\hole -> lf $ branch xr (branch x tl trl) hole) trr

-- | Smart constructor for making a branching node that generates
--   a cached measurement.
branch :: Measured s a => a -> Splay s a -> Splay s a -> Splay s a
branch x tl tr = Branch (measure tl <> measure x <> measure tr) x tl tr

-- | Construct a sequence of only one element
singleton :: Measured s a => a -> Splay s a
singleton x = branch x Leaf Leaf

findAndSplay :: forall s a. Measured s a =>
                (s -> Bool) -> Splay s a -> Splay s a
findAndSplay f t = go (mempty :: s) id id t
  where
    ms :: forall m v. Measured m v => v -> m
    ms = measure

    -- Don't worry, this just makes GHC happy =P.
    go :: s
       -> (Splay s a -> Splay s a)
       -> (Splay s a -> Splay s a)
       -> (Splay s a -> Splay s a)
          
    -- Found at current node
    go pre lf rf (Branch _ x tl tr)
      | not (f $ pre <> ms tl)  && f (pre <> ms tl <> ms x) =
          branch x (lf tl) (rf tr)

    -- Zig left
    go pre lf rf (Branch _ x
                  (Branch _ xl tll tlr) tr)
      
      | not (f $ pre <> ms tll) && f (pre <> ms tll <> ms xl) =
          branch xl (lf tll) (rf $ branch x tlr tr)

    -- Zig right
    go pre lf rf (Branch _ x tl
                  (Branch _ xr trl trr))
      | not (f $ pz <> ms trl) && f (pz <> ms trl <> ms xr) =
          branch xr (lf $ branch x tl trl) (rf trr)
      where pz = pre <> ms tl <> ms x

    -- Zig-Zig left
    go pre lf rf (Branch _ x
                  (Branch _ xl
                   tll@(Branch _ xll tlll _) trl) tr)
      | f (pre <> ms tlll <> ms xll) =
          go pre lf (\hole -> rf $ branch xl hole $ branch x trl tr) tll

    -- Zig-Zig right
    go pre lf rf (Branch _ x tl
                  (Branch _ xr trl
                   trr@(Branch _ xrr trrl _)))
      | not (f pz) && f (pz <> ms trrl <> ms xrr) =
          go pz (\hole -> lf $ branch xr (branch x tl trl) hole) rf trr
      where pz = pre <> ms tl <> ms x <> ms trl <> ms xr

    -- Zig-Zag left-right
    go pre lf rf (Branch _ x
                  (Branch _ xl tll
                   tlr@(Branch _ xlr tlrl _))
                  tr)
      
      | not (f pz) && f (pz <> ms tlrl <> ms xlr) =
          go pz (\hl -> lf $ branch xl tll hl)
                (\hr -> rf $ branch x hr tr) tlr
      where pz = pre <> ms tll <> ms xl

    -- Zig-Zag right-left
    go pre lf rf (Branch _ x tl
                  (Branch _ xr
                   trl@(Branch _ xrl trll _) trr))
      | not (f pz) && f (pz <> ms trll <> ms xrl) =
          go pz (\hl -> lf $ branch x tl hl)
                (\hr -> rf $ branch xr hr trr) trl
      where pz = pre <> ms tl <> ms x

    -- If this happened then something went wrong
    go _ _ _ _ =
      error "splay: Invalid arguments, inconsistent monoid or internal error"

-- | Split the sequence into two part: the longest prefix whose measure does
--   /not/ satisfy the predicate, and the rest of the sequence
--
--   The predicate must be monotonic. Note that @'False' < 'True'@.
split :: Measured s a => (s -> Bool) -> Splay s a -> (Splay s a, Splay s a)
split f t | not (f mempty || f (measure t)) = (t, Leaf)
          | f mempty && f (measure t) = (t, Leaf)
split f t = case findAndSplay f t of
  Branch _ x tl tr -> (tl, branch x Leaf tr)
  Leaf -> error "splay: internal error"

-- | Traverse the sequence
--
--   @
--   traverseSplay :: (Measured sa a, Measured sb b)
--                 => Traversal a b (Splay sa a) (Splay sb b)
--   @
traverseSplay :: (Measured sa a, Measured sb b, Applicative f)
                 => (a -> f b)
                 -> Splay sa a -> f (Splay sb b)
traverseSplay _ Leaf = pure Leaf
traverseSplay f (Branch _ x tl tr) = branch <$> f x
                                            <*> traverseSplay f tl
                                            <*> traverseSplay f tr

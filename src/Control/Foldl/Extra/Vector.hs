{-# LANGUAGE AllowAmbiguousTypes, BangPatterns, DeriveFunctor, DerivingVia #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, FunctionalDependencies   #-}
{-# LANGUAGE GADTs, LambdaCase, MultiParamTypeClasses, PatternSynonyms     #-}
{-# LANGUAGE QuantifiedConstraints, RankNTypes, ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications, TypeFamilies, TypeOperators                 #-}
{-# LANGUAGE UndecidableInstances                                          #-}
-- | Provides folds for accumulation on vectors.
module Control.Foldl.Extra.Vector
  (accumM, accumWithM, accum, accumWith) where
import           Control.Foldl               as Fl
import           Control.Monad.Primitive
import           Control.Monad.ST
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as MG

accumM
  :: (PrimMonad m, MG.MVector v a)
  => v (PrimState m) a
  -> FoldM m (Int, a) ()
accumM mv = Fl.mapM_ step
  where
    step (!i, !x) = MG.write mv i x

accumWithM
  :: (PrimMonad m, MG.MVector v a)
  => (a -> b -> a)
  -> v (PrimState m) a
  -> FoldM m (Int, b) ()
accumWithM f mv = Fl.mapM_ step
  where
    step (!i, !y) = do
      x <- MG.read mv i
      MG.write mv i $! f x y

accum
  :: G.Vector v a
  => v a
  -> Fold (Int, a) (v a)
accum v = Fold step (G.new $ G.clone v) id
  where
    step v' (!i, !x) = runST $ do
      mv <- G.unsafeThaw v'
      MG.write mv i x
      return v'

accumWith
  :: G.Vector v a
  => (a -> b -> a)
  -> v a
  -> Fold (Int, b) (v a)
accumWith f v = Fold step (G.new $ G.clone v) id
  where
    step v' (!i, !y) = runST $ do
      mv <- G.unsafeThaw v'
      x <- MG.read mv i
      MG.write mv i $! f x y
      return v'

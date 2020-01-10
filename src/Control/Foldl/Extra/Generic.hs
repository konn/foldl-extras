{-# LANGUAGE AllowAmbiguousTypes, BangPatterns, DefaultSignatures          #-}
{-# LANGUAGE DeriveFunctor, DerivingVia, FlexibleContexts                  #-}
{-# LANGUAGE FlexibleInstances, FunctionalDependencies, GADTs, LambdaCase  #-}
{-# LANGUAGE MultiParamTypeClasses, PatternSynonyms, QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables, TypeApplications             #-}
{-# LANGUAGE TypeFamilies, TypeOperators, UndecidableInstances             #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
-- | Provides generic abstractions over
--   @'Fl.Fold'@ and @'Fl.FoldM'@.
module Control.Foldl.Extra.Generic
  ( Folder(..), Rep', foldIt, finish
  ) where
import           Control.Arrow
import           Control.Comonad
import           Control.Foldl         as Fl
import           Control.Monad.ST
import qualified Data.Bifunctor        as B
import           Data.Coerce
import           Data.Functor.Identity
import           Data.Monoid           (Endo (..))
import           Data.Profunctor
import           Data.Profunctor.Rep
import           Data.Profunctor.Sieve

class (Profunctor fold, Representable (Arr fold))
   => Folder fold where
  type Arr fold :: * -> * -> *
  runFoldOver :: HandlerM (Rep' fold) x a -> fold a b -> Arr fold x b
  handle :: HandlerM (Rep' fold) a b -> Arr fold (fold b r) (fold a r)
  runFold' :: Foldable t => fold a b -> Arr fold (t a) b
  default runFold'
    :: (Foldable t, Monad (Rep' fold))
    => fold a b -> Arr fold (t a) b
  runFold' = runFoldOver folded
  suspend   :: fold a b -> fold a (fold a b)
  finish'  :: Arr fold (fold a b) b
  fromFoldUnwrap
    :: (forall x. (x -> a -> Rep' fold x) -> Rep' fold x -> (x -> Rep' fold b) -> r)
    -> fold a b -> Rep' fold r

genEndo :: Endo a -> EndoM Identity a
genEndo = coerce

simpHandler
  :: HandlerM Identity a b -> Handler a b
simpHandler h =  \f -> fmap (B.first coerce) $
    h (B.first (fmap genEndo) . f)

instance Folder Fold where
  type Arr Fold = (->)
  handle h = handles $ simpHandler h
  runFoldOver h = Fl.foldOver $ simpHandler h
  {-# INLINE runFoldOver #-}
  runFold' = Fl.fold
  {-# INLINE runFold' #-}
  suspend = duplicate
  {-# INLINE suspend #-}
  finish' = extract
  {-# INLINE finish' #-}
  fromFoldUnwrap f =
    pure .
      purely (\step ini ->
      f (rmap Identity . step) (Identity ini)
      . rmap Identity)
  {-# INLINE fromFoldUnwrap #-}

instance Monad m => Folder (FoldM m) where
  {-# SPECIALISE instance Folder (FoldM Identity) #-}
  {-# SPECIALISE instance Folder (FoldM IO) #-}
  {-# SPECIALISE instance Folder (FoldM (ST s)) #-}
  type Arr (FoldM m) = Kleisli m
  runFoldOver h = fmap Kleisli $ foldOverM h
  {-# INLINE runFoldOver #-}
  handle h = Kleisli $ pure . handlesM h
  {-# INLINE handle #-}
  runFold' = Kleisli . Fl.foldM
  {-# INLINE runFold' #-}
  suspend = duplicateM
  {-# INLINE suspend #-}
  finish' = Kleisli $ \(FoldM _ ini ext) -> ext =<< ini
  {-# INLINE finish' #-}
  fromFoldUnwrap f =
    pure . impurely (\step ini ext -> f step ini ext)
  {-# INLINE fromFoldUnwrap #-}

type Rep' fold = Rep (Arr fold)

foldIt
  :: (Folder fold, Foldable t)
  => fold a b -> t a -> Rep' fold b
foldIt = sieve . runFold'
{-# INLINE foldIt #-}
{-# SPECIALISE INLINE
    foldIt :: Foldable t => Fold a b -> t a -> Identity b
  #-}
{-# SPECIALISE INLINE
    foldIt :: (Monad m, Foldable t) => FoldM m a b -> t a -> m b
  #-}

finish
  :: (Folder fold) => fold a b -> Rep' fold b
finish = sieve finish'
{-# INLINE finish #-}
{-# SPECIALISE INLINE
    finish :: Fold a b -> Identity b
  #-}
{-# SPECIALISE INLINE
    finish :: (Monad m) => FoldM m a b -> m b
  #-}
{-# SPECIALISE INLINE
    finish :: FoldM IO a b -> IO b
  #-}
{-# SPECIALISE INLINE
    finish :: FoldM Identity a b -> Identity b
  #-}
{-# SPECIALISE INLINE
    finish :: FoldM (ST s) a b -> ST s b
  #-}

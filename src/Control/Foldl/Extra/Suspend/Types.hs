{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE TypeFamilies, UndecidableInstances                      #-}
module Control.Foldl.Extra.Suspend.Types
  ( Suspended(..)
  ) where
import Control.Foldl.Extra.Generic
import Data.Coerce
import Data.Profunctor
import Data.Profunctor.Rep
import Data.Profunctor.Sieve

newtype Suspended fold a b = Suspended { runSuspended :: Rep' fold (fold a b) }

instance (Functor (Rep' fold), Profunctor fold) => Profunctor (Suspended fold) where
  dimap f g = coerce $ fmap @(Rep' fold) $ dimap @fold f g

instance (Monad (Rep' fold), Folder fold) => Folder (Suspended fold) where
  type Arr (Suspended fold) = Arr fold
  runFold' (Suspended mfold) = tabulate $ \t -> do
    fld <- mfold
    foldIt fld t
  {-# INLINE runFold' #-}
  suspend = Suspended . fmap (rmap (Suspended . pure) . suspend) . runSuspended
  {-# INLINE suspend #-}
  finish' = tabulate $ \(Suspended mf) -> do
    f <- mf
    finish f
  {-# INLINE finish' #-}
  fromFoldUnwrap f (Suspended mf) = do
    fld <- mf
    fromFoldUnwrap f fld
  {-# INLINE fromFoldUnwrap #-}

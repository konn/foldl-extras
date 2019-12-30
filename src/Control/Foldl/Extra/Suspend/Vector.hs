{-# LANGUAGE FlexibleContexts, GADTs, ScopedTypeVariables, TypeApplications #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Control.Foldl.Extra.Suspend.Vector where
import Control.Foldl.Extra.Generic
import Control.Foldl.Extra.Suspend.Types
import Control.Monad
import Data.Profunctor
import Data.Vector.Generic               as G

data WrapVector v a b where
  WrapVector :: !(v a) -> WrapVector v a a

instance G.Vector v a => Foldable (WrapVector v a) where
  foldr = \f z (WrapVector vec) -> G.foldr' f z vec
  {-# INLINE foldr #-}

flat
  :: Monad m => m (a -> m b)
  -> a -> m b
flat mf = join . ap mf . pure

(<+<)
  :: forall fold a b v.
      (Folder fold, G.Vector v a)
  => fold a b -> v a -> Suspended fold a b
(<+<) = dimap WrapVector Suspended . foldIt . suspend
infixl 9 <+<

($$)
  :: forall fold a b v. (Folder fold, G.Vector v a)
  => fold a b -> v a -> Rep' fold b
($$) = lmap WrapVector . foldIt
infixr 0 $$


{-# LANGUAGE GADTs, LambdaCase #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Control.Foldl.Extra.Mono
  (WrapMono(..), (<+<), ($$)
  ) where
import Control.Foldl.Extra.Generic
import Control.Foldl.Extra.Suspend.Types
import Data.MonoTraversable
import Data.Profunctor

data WrapMono f a where
  WrapMono :: !mono -> WrapMono mono (Element mono)

instance MonoFoldable mono => Foldable (WrapMono mono) where
  foldMap f = \case
    WrapMono mono -> ofoldMap f mono
  {-# INLINE foldMap #-}
  foldr = \f z (WrapMono m) -> ofoldr f z m
  {-# INLINE foldr #-}

(<+<)
  :: (MonoFoldable mono, Folder fold)
  => fold (Element mono) b -> mono -> Suspended fold (Element mono) b
(<+<) = dimap WrapMono Suspended . foldIt . suspend
{-# INLINE (<+<) #-}
infixl 9 <+<

($$)
  :: (MonoFoldable mono, Folder fold)
  => fold (Element mono) b -> mono -> Rep' fold b
($$) = lmap WrapMono . foldIt
infixr 0 $$

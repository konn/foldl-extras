{-# LANGUAGE DeriveFunctor, DerivingStrategies, DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables, TupleSections, TypeApplications          #-}
{-# LANGUAGE TypeFamilies                                                  #-}
module Control.Foldl.Extra.Abort where
import           Control.Arrow
import           Control.Foldl               (FoldM, handlesM)
import qualified Control.Foldl               as L
import           Control.Foldl.Extra.Generic
import           Control.Lens
import           Data.Profunctor.Rep

foldAbort
  :: Foldable t
  => FoldM (Either e) a b
  -> t a
  -> Either e b
foldAbort = L.foldM

foldAbortOver
  :: L.HandlerM (Either e) x a
  -> FoldM (Either e) a b
  -> x
  -> Either e b
foldAbortOver = L.foldOverM

foldShortcut
  :: Foldable t
  => FoldM (Either b) a b
  -> t a
  -> b
foldShortcut = fmap (either id id) . L.foldM

-- | Pure strict left-fold, which can abort with a value e.
newtype FoldAbort e a b = FoldAbort { runFoldAbort :: FoldM (Either e) a b }
  deriving (Functor, Profunctor)
  deriving newtype (Applicative)

instance Folder (FoldAbort e) where
  type Arr (FoldAbort e) = Kleisli (Either e)
  runFoldOver h (FoldAbort f) = runFoldOver h f
  {-# INLINE runFoldOver #-}
  handle h = tabulate $ \(FoldAbort f) ->
    return $ FoldAbort $ handlesM h f
  {-# INLINE handle #-}
  suspend = FoldAbort . fmap FoldAbort . suspend . runFoldAbort
  {-# INLINE suspend #-}
  finish' = tabulate $ finish . runFoldAbort
  fromFoldUnwrap f = fromFoldUnwrap f . runFoldAbort

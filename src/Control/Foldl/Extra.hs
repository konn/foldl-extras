{-# LANGUAGE AllowAmbiguousTypes, BangPatterns, DeriveFunctor, DerivingVia #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, FunctionalDependencies   #-}
{-# LANGUAGE GADTs, LambdaCase, MultiParamTypeClasses, PatternSynonyms     #-}
{-# LANGUAGE QuantifiedConstraints, RankNTypes, ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications, TypeFamilies, TypeOperators                 #-}
{-# LANGUAGE UndecidableInstances                                          #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Control.Foldl.Extra where
import Control.Foldl.Extra.Generic
import Control.Foldl.Extra.Mono
import Data.MonoTraversable
import Data.Profunctor

foldMono
  :: (Folder fold, MonoFoldable mono) => fold (Element mono) b -> mono -> Rep' fold b
{-# INLINE foldMono #-}
foldMono = lmap WrapMono . foldIt

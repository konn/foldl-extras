module Control.Foldl.Extra.Suspend where
import Control.Foldl.Extra.Generic
import Control.Foldl.Extra.Suspend.Types

{- |
Feed stream and wait for future input.

@
 ('>+>') = 'Fl.fold' . 'duplicate'
       :: ('Monad' m, 'Foldee' a seq) => 'Fold' a b -> seq -> 'Fold' a b
 ('>+>') = 'Fl.foldM' . 'duplicateM'
       :: ('Monad' m, 'Foldee' a seq) => 'FoldM' m a b -> seq -> m ('FoldM' m a b)
@
-}
(<+<)
  :: (Folder fold, Foldable t)
  => fold a b -> t a -> Suspended fold a b
(<+<) = fmap Suspended . foldIt . suspend
infixl 9 <+<

($$)
  :: (Folder fold, Foldable t)
  => fold a b -> t a -> Rep' fold b
($$) = foldIt
infixr 0 $$

# foldl-extras

A utility package to work more confortably with [`foldl`][foldl] package.

- Resumable streaming, much looks alike those in [`conduit`][conduit] or [`pipes`][pipes];
- Specialised interfaces for [`mono-traversable`][mono-traversable]s and [`vector`][vector]s.

[foldl]: https://hackage.haskell.org/package/foldl
[pipes]: https://hackage.haskell.org/package/pipes
[conduit]: https://hackage.haskell.org/package/conduit
[mono-traversable]: https://hackage.haskell.org/package/mono-traversable
[vector]: https://hackage.haskell.org/package/vector

## Examples

```haskell
ghci> Fl.sum <+< [1,2,3] $$ Set.fromList [5,12,9,5]
Identity 32
ghci> let v = V.fromList [1,2,3,4,5] :: V.Vector Int
ghci> finish $  accumWith (*) v <+< [(0, 2)] <+< Set.fromList [(3,5)]
Identity [2,2,3,20,5]
ghci> :{
ghci|  create $ do
ghci|    mv <- unsafeThaw [1..10]
ghci|    accumWithM (*) mv <+< [(0, 2)] $$ Set.fromList [(3,5)]
ghci|    return mv
ghci| :}
[2,2,3,20,5,6,7,8,9,10]
```

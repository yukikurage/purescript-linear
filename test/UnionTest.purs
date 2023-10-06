module Test.UnionTest where

import Prelude

import Linear.Internal (Unique)
import Prim.Row (class Union)
import Type.Proxy (Proxy(..))

data Prox :: Unique -> Type
data Prox k = Prox

test :: forall u s. Union (hoge :: u) (fuga :: s) (fuga :: s, hoge :: u) => Prox u -> Prox s -> Unit
test _ _ = unit

make :: (forall r u. Prox u -> Prox r -> Unit) -> Unit
make f = f Prox Prox

main = make test

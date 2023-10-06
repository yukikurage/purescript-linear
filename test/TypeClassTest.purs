module Test.TypeClassTest where

import Prelude

import Prim.Row (class Cons)
import Type.Equality (class TypeEquals)
import Type.Proxy (Proxy(..))

class Never

class Test a b

instance Never => Test a a
else instance Test a b

x :: forall @l r. Cons l Int () r => Proxy r
x = Proxy

-- y :: forall r. Cons "a" Int () r => Proxy r
-- y = x

y :: forall l r. TypeEquals l "a" => Cons l Int () r => Proxy r
y = x @l

-- | This fails
y' :: forall l r. Cons "a" Int () r => Proxy r
y' = x @"a"

z :: forall m. Monoid m => Proxy m
z = Proxy

w :: forall m. Monoid m => Proxy m
w = z

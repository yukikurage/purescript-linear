module Linear
  ( (<$>.)
  , (<*>.)
  , applyL
  , bindL
  , class Max
  , fin
  , make
  , mapL
  , pureL
  , run
  , subL
  ) where

import Prelude

import Linear.Internal (type (-.), FunctionL(..), Linear(..))

import Data.Symbol (class IsSymbol)
import Prim.Row (class Cons, class Nub, class Union)
import Prim.Int (class Add, class Compare, class ToString)
import Prim.Ordering (GT, LT)

class Max :: Int -> Int -> Int -> Constraint
class Max i j k | i j -> k

instance Max i i i
else instance Compare i j LT => Max i j j
else instance Compare i j GT => Max i j i

run :: forall a b. (a -. b) -> a -> b
run (FunctionL f) = f

mapL :: forall a b i r. (a -. b) -> Linear i r a -> Linear i r b
mapL (FunctionL f) (Linear x) = Linear \_ -> (f $ x unit)

infixl 4 mapL as <$>.

applyL :: forall a b i j k r s t. Max i j k => Union r s t => Nub t t => Linear i r (a -. b) -> Linear j s a -> Linear k t b
applyL (Linear f) (Linear x) = Linear \_ -> case f unit of FunctionL g -> g $ x unit

bindL :: forall a b i j k r s t. Max i j k => Union r s t => Nub t t => Linear i r a -> (a -. Linear j s b) -> Linear k t b
bindL (Linear x) (FunctionL f) = Linear \_ -> case f $ x unit of Linear g -> g unit

infixl 4 applyL as <*>.

make
  :: forall i iPlusOne l r s t u a b
   . IsSymbol l
  => Add i 1 iPlusOne
  => ToString iPlusOne l
  => Cons l Unit () t
  => Cons l Unit r u
  => Cons l (Linear iPlusOne t a) () s
  => (Linear iPlusOne t a -> Linear iPlusOne u b)
  -> Linear i r (a -. b)
make f = Linear \_ -> FunctionL \a -> case f (Linear \_ -> a) of Linear b -> b unit

fin :: forall a. Linear 0 () a -> a
fin (Linear a) = a unit

pureL :: forall a. a -> Linear 0 () a
pureL a = Linear \_ -> a

subL :: Int -. Int -. Int
subL = FunctionL \x -> FunctionL \y -> x - y

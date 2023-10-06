module Linear
  ( (<$>.)
  , (<*>.)
  , applyL
  , bindL
  , fin
  , make
  , mapL
  , pureL
  , run
  , subL
  ) where

import Prelude

import Data.Symbol (class IsSymbol)
import Linear.Internal (type (-.), FunctionL(..), Linear(..))
import Prim.Row (class Cons, class Nub, class Union)
import Prim.RowList (RowList)

-- カインド Unique の値は forall で導入
foreign import data Unique :: Type

-- 与えられた Row に対して、指定されたラベルが存在するか判定する
class Available :: Row Type -> Symbol -> Constraint
class Available r a

run :: forall a b. (a -. b) -> a -> b
run (FunctionL f) = f

mapL
  :: forall a b r
   . (a -. b)
  -> Linear r a
  -> Linear r b
mapL (FunctionL f) (Linear x) = Linear \_ -> (f $ x unit)

infixl 4 mapL as <$>.

applyL
  :: forall a b r s t
   . Union r s t
  => Nub t t
  => Linear r (a -. b)
  -> Linear s a
  -> Linear t b
applyL (Linear f) (Linear x) = Linear \_ -> case f unit of FunctionL g -> g $ x unit

bindL
  :: forall a b r s t
   . Union r s t
  => Nub t t
  => Linear r a
  -> (a -. Linear s b)
  -> Linear t b
bindL (Linear x) (FunctionL f) = Linear \_ -> case f $ x unit of Linear g -> g unit

infixl 4 applyL as <*>.

make
  :: forall l r s t u a b
   . IsSymbol l
  => Cons l Unit () t
  => Cons l Unit r u
  => Cons l (Linear t a) () s
  => (Linear t a -> Linear u b)
  -> Linear r (a -. b)
make f = Linear \_ -> FunctionL \a -> case f (Linear \_ -> a) of Linear b -> b unit

fin :: forall a. Linear () a -> a
fin (Linear a) = a unit

pureL :: forall a. a -> Linear () a
pureL a = Linear \_ -> a

subL :: Int -. Int -. Int
subL = FunctionL \x -> FunctionL \y -> x - y

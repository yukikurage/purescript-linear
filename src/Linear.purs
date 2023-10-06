module Linear
  ( (<$>.)
  , (<*>.)
  , applyL
  , fin
  , make
  , mapL
  , pureL
  , run
  , subL
  ) where

import Prelude

import Linear.Internal (class Disjoint, class NotElemProof, type (-.), FunctionL(..), Linear(..))
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)

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
  => Disjoint r s
  => Linear r (a -. b)
  -> Linear s a
  -> Linear t b
applyL (Linear f) (Linear x) = Linear \_ -> case f unit of FunctionL g -> g $ x unit

infixl 4 applyL as <*>.

make
  :: forall r a b
   . (forall u. NotElemProof r u => Linear (uniq :: u) a -> Linear (uniq :: u | r) b)
  -> Linear r (a -. b)
make f = Linear \_ -> FunctionL \a -> case forceCoerce f unit (Linear \_ -> a) of Linear b -> b unit
  where
  forceCoerce :: forall s t. (forall u. NotElemProof r u => Linear (uniq :: u) a -> Linear (uniq :: u | r) b) -> Unit -> Linear s a -> Linear t b
  forceCoerce = unsafeCoerce

fin :: forall a. Linear () a -> a
fin (Linear a) = a unit

pureL :: forall a. a -> Linear () a
pureL a = Linear \_ -> a

subL :: Int -. Int -. Int
subL = FunctionL \x -> FunctionL \y -> x - y

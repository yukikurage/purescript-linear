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

import Linear.Internal (class AppendRowList, class DeleteUniversal, class Max, type (-.), FunctionL(..), Linear(..))
import Prim.Int (class Add, class ToString)
import Prim.RowList as RL
import Unsafe.Coerce (unsafeCoerce)

run :: forall a b. (a -. b) -> a -> b
run (FunctionL f) = f

mapL
  :: forall i a b r
   . (a -. b)
  -> Linear i r a
  -> Linear i r b
mapL (FunctionL f) (Linear x) = Linear \_ -> (f $ x unit)

infixl 4 mapL as <$>.

applyL
  :: forall i j k a b r s t
   . Max i j k
  => AppendRowList r s t
  => Linear i r (a -. b)
  -> Linear j s a
  -> Linear k t b
applyL (Linear f) (Linear x) = Linear \_ -> case f unit of FunctionL g -> g $ x unit

infixl 4 applyL as <*>.

make
  :: forall i iPlusOne label r a b
   . ToString iPlusOne label
  => Add i 1 iPlusOne
  => ( forall univ r'
        . Linear iPlusOne (RL.Cons label univ RL.Nil) a
       -> Linear iPlusOne r' b
     )
  -> Linear i r (a -. b)
make f = Linear
  \_ -> FunctionL $ forceCoerce f unit
  where
  forceCoerce
    :: ( forall univ r'
          . DeleteUniversal label univ r' r
         => Linear iPlusOne (RL.Cons label univ RL.Nil) a
         -> Linear iPlusOne r' b
       )
    -> Unit
    -> a
    -> b
  forceCoerce = unsafeCoerce

fin :: forall a. Linear 0 RL.Nil a -> a
fin (Linear a) = a unit

pureL :: forall a. a -> Linear 0 RL.Nil a
pureL a = Linear \_ -> a

subL :: Int -. Int -. Int
subL = FunctionL \x -> FunctionL \y -> x - y

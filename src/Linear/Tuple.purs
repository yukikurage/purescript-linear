module Linear.Tuple
  ( (/\.)
  , TupleL
  , matchTupleL
  , tupleL
  , type (/\.)
  ) where

import Linear.Internal (type (-.), FunctionL(..))

data TupleL a b = TupleL a b

infixr 6 type TupleL as /\.

tupleL :: forall a b. a -. b -. TupleL a b
tupleL = FunctionL \a -> FunctionL \b -> TupleL a b

infixr 6 tupleL as /\.

matchTupleL :: forall a b r. TupleL a b -. (a -. b -. r) -. r
matchTupleL = FunctionL \(TupleL a b) -> FunctionL \(FunctionL f) -> case f a of FunctionL g -> g b

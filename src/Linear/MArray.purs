module Linear.MArray where

import Data.Maybe (Maybe(..))
import Linear.Internal (type (-.))
import Linear.Tuple (type (/\.), TupleL, (/\.))
import Linear.Ur (Ur)

-- 線型型を使った配列の管理

foreign import data MArray :: Type -> Type

foreign import withArray :: forall a b. Array a -> (MArray a -. Ur b) -> Ur b

foreign import freeze :: forall a. MArray a -. Ur (Array a)

foreign import set :: forall a. Int -. a -. MArray a -. MArray a

foreign import push :: forall a. a -. MArray a -. MArray a

foreign import getImpl
  :: forall a
   . (forall b c. b -. c -. TupleL b c)
  -> (forall b. b -> Maybe b)
  -> (forall b. Maybe b)
  -> Int -. MArray a -. Ur (Maybe a) /\. MArray a

get :: forall a. Int -. MArray a -. Ur (Maybe a) /\. MArray a
get = getImpl (/\.) Just Nothing

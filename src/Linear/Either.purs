module Linear.Either where

import Linear.Internal (type (-.), FunctionL(..))

data EitherL a b = LeftL a | RightL b

infixr 5 type EitherL as \/.

leftL :: forall a b. a -. EitherL a b
leftL = FunctionL LeftL

rightL :: forall a b. b -. EitherL a b
rightL = FunctionL RightL

matchEitherL :: forall a b c. (a -. c) -> (b -. c) -> EitherL a b -. c
matchEitherL (FunctionL f) (FunctionL g) = FunctionL case _ of
  LeftL a -> f a
  RightL b -> g b
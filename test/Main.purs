module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)
import Linear (fin, make, pureL, subL, (<$>.), (<*>.))
import Linear.Internal (type (-.))
import Linear.MArray (freeze, set, withArray)
import Linear.Ur (Ur(..))

main :: Effect Unit
main = do
  let Ur x = arrayTest
  log $ show x

arrayTest :: Ur (Array Int)
arrayTest = withArray [ 0, 1, 2 ] $ fin $
  make \arr ->
    let
      _ = withArray [] $ fin $ make \_ -> freeze <$>. (set <$>. pureL 1 <*>. pureL 20 <*>. arr)
    in
      freeze <$>. (set <$>. pureL 0 <*>. pureL 20 <*>. arr)

test :: Int -. Int -. Int
test = fin $ make \x -> make \y ->
  subL <$>. x <*>. y

module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)
import Linear (fin, make, pureL, subL, (<$>.), (<*>.))
import Linear.Internal (class DeleteUniversal, type (-.), Linear)
import Linear.MArray (freeze, set, withArray)
import Linear.Ur (Ur(..))
import Prim.Row (class Cons)
import Prim.RowList as RL

main :: Effect Unit
main = do
  let Ur x = arrayTest
  log $ show x

arrayTest :: Ur (Array Int)
arrayTest = withArray [ 0, 1, 2 ] $ fin $
  make \arr -> freeze <$>. (set <$>. pureL 0 <*>. pureL 20 <*>. arr)

test :: Int -. Int -. Int
test = fin $ make f
  where
  f
    :: forall univ r
     . DeleteUniversal "1" univ r RL.Nil
    => Linear 1 (RL.Cons "1" univ RL.Nil) Int
    -> Linear 1 r (Int -. Int)

  f x = make g
    where
    g
      :: forall univ' r''
       . DeleteUniversal "2" univ' r' r
      => Linear 2 (RL.Cons "2" univ' RL.Nil) Int
      -> Linear 2 r' Int
    g y = subL <$>. x <*>. y

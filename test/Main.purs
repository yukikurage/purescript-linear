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
  make \arr -> freeze <$>. (set <$>. pureL 0 <*>. pureL 20 <*>. arr)

test :: Int -. Int -. Int
test = fin $ make \x -> make \y ->
  subL <$>. x <*>. y

-- 次のやつはちゃんとコンパイルエラーになる
{-

test :: Int -. Int -. Int
test = fin $
  make @"x" \x ->
    make @"y" \y ->
      subL `mapL` x `applyL` x -- x の二回適用
-}

-- 同じ名前を付けても何故かうまくコンパイルエラーが出る　どこで出てるんだ？
{-
 Could not match type

    ( ... )

  with type

    ( x :: Unit
    ...
    )
-}

{-

test2 :: Int -. Int -. Int
test2 = fin $
  make @"x" \x ->
    make @"x" \y ->
      subL `mapL` x `applyL` lift 10

-}

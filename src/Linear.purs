module Linear
  ( LFunction
  ) where

newtype LFunction a b = LFunction (a -> b)

infixr 0 type LFunction as -.

-- make :: (Linear r (X | r) a -> Linear s (X | s) b) -> Linear r s (a -. b)

-- run :: (a -. b) -> (Linear r s a -> Linear r s b)

-- fin :: Linear () () a -> a

{-

メモ

-- これだと問題がある
make :: (a -> b) -> (a -. b)

-- 問題1. Linear関数意外にも適用出来てしまう
make (\x -> x + x)

-- 上記問題を特別な型Linear :: Type -> Typeで解決する
run :: (a -. b) -> (Linear a -> Linear b)
make :: (Linear a -> Linear b) -> (a -. b)

-- LinearをMonadにすれば複数引数っぽいのも作れる

-- 問題2. 線形制約が付けられない

plusL :: Int -. (Int -. Int)

run plusL :: Linear Int -> Linear (Int -. Int)

join :: Linear (Linear a) -> Linear a

-- x を二回使う関数
f :: Int -. (Int -. Int)
f = make \(x :: Linear a) -> make \(y :: Linear a) -> run plusL x >>= \g -> run g x

-- run でこの変数を使ったというのを記録して、makeでそれを消費する、みたいに出来ないか
run :: (a -. b) -> (Linear (X) a -> Linear (X) b)

-- こうすると上記 f 内の g は次のようになる
g :: Linear (X) (Int -. Int)

-- Indexed Monad?
-- これをエラーにしたい
bind :: (Linear (X) (Int -. Int)) -> ((Int -. Int) -> Linear (X) Int) -> Linear (X) Int

-- 一方これはOK こういうのは作れるはず
bind :: (Linear (X) (Int -. Int)) -> ((Int -. Int) -> Linear (Y) Int) -> Linear (X, Y) Int

-- そして make はこんな感じ
make :: (Linear (Y) Int -> Linear (X, Y)) -> Linear (X) (Int -. Int)

fin :: Linear () a -> a

f :: Int -. (Int -. Int)
f = fin $ make \(x :: Linear a) -> make \(y :: Linear a) -> run plusL x >>= \g -> run g x

-- indexed Monad にしてはどうか
ibind :: forall a b x y z. m x y a -> (a -> m y z b) -> m x z b

make :: (Linear r (X | r) a -> Linear s (X | s) b) -> Linear r s (a -. b)

run :: (a -. b) -> (Linear r s a -> Linear r s b)

fin :: Linear () () a -> a

-- 関数じゃなくて Linear だけでできる？もしかして
-- 例えば次のようなのが線形な関数
Linear r (X | r) a -> Linear r (X | r) b

-- つまりこれ
Linear r s a -> Linear r s b

plusL :: Linear r s Int -> Linear r s (forall t u. Linear t u Int -> Linear t u Int)

f :: forall r s. Linear r s Int -> Linear r s (forall t u. Linear t u Int -> Linear t u Int)
f (xL :: Linear r s Int) = xL <#> \(x :: Int) -> g
  where
    g :: forall t u. Linear t u Int -> Linear t u Int
    g



Linear r s a は Functor では**ない**

(a -> b) -> Linear r s a -> Linear r s b

とかいう操作、明らかにやばいので

Linear r s a は

「計算結果は a 型、リソースとして r を取って計算した後のリソースが s」

であるような計算　つまり r > s 的なものにする

これを使うと、例えば 2 引数取って 2 つめから 1 つめを引く関数を作りたい時……

なお、2引数関数は次のようになる？

forall r s. Linear r s a -> Linear r s (forall t u. Linear t u b -> Linear t u c)

subL :: Linear r s Int -> Linear r s (forall t u. Linear t u Int -> Linear t u Int)

関数適用用の関数なども用意

applyL :: Linear r s (forall t u. Linear t u a -> Linear t u b) -> Linear v r a -> Linear v s b

joinL :: Linear r s (Linear s t a) -> Linear r t a

-- f :: Linear r s Int -> Linear r s (Linear t u Int -> Linear t u Int)
-- f xL =
-}

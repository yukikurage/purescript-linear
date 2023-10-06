module Linear.Internal where

import Prelude

import Prim.Int (class Add)
import Prim.RowList as RL
import Type.Equality (class TypeEquals)

newtype FunctionL a b = FunctionL (a -> b)

-- | 全称量化された値を表す
foreign import data Universal :: Type

-- | 遅れて評価される（ST 的な）
-- | これを評価できるのは fin のみ（だと思う……）
-- | これにより、letなどで一時的に束縛され返り値に使われない場合はスキップできる（はず）
-- | level : make のネスト回数（ただし、make で生成された、関数を Linear で包んだ物をどこでも使わなかった場合、同じになる場合がある）
-- | deps : 依存する値のリスト　ラベルはその引数が導入された level を ToString したもの。Universal は導入時に量化されたもの
-- |    label は次の性質を持つ : 別の階層の引数は基本的に同じ値を持たない。しかし、make によって生成した関数をどこでも使わなかった場合重複する可能性がある
-- |    Universal は次の性質を持つ : 引数は全て別の値を持つ。**しかし、コンパイラは2つの Universal が別の値を持つことを確定出来ない**（ややこしいが、同じであることは判定できる）
-- | まず label をマッチし、その後 Universal で同値判定を行う事で、引数が同じであることを確定できる
newtype Linear :: Int -> RL.RowList Universal -> Type -> Type
newtype Linear level deps a = Linear (Unit -> a)

infixr 0 type FunctionL as -.

-- | deps から
-- | - 最初に見つかった label の値 x が
-- | - 渡された univ と同じであった場合（これは判定可能）
-- | - そのペアを deps から削除した物を返す
-- | これ以外の場合は失敗する
class DeleteUniversal :: Symbol -> Universal -> RL.RowList Universal -> RL.RowList Universal -> Constraint
class DeleteUniversal label univ deps newDeps | label univ deps -> newDeps

instance DeleteUniversal label univ (RL.Cons label univ deps) deps
else instance
  ( DeleteUniversal label univ deps newDeps
  , TypeEquals (RL.Cons label' univ' newDeps) out
  ) =>
  DeleteUniversal label univ (RL.Cons label' univ' deps) out

-- | RowList を結合する便利クラス
class AppendRowList :: forall a. RL.RowList a -> RL.RowList a -> RL.RowList a -> Constraint
class AppendRowList a b c | a b -> c

instance TypeEquals b out => AppendRowList RL.Nil b out
instance
  ( AppendRowList a b c
  , TypeEquals (RL.Cons label univ c) out
  ) =>
  AppendRowList (RL.Cons label univ a) b out

-- | 与えられた二つの Int のうち大きい方を返す便利クラス
class Max :: Int -> Int -> Int -> Constraint
class Max a b c | a b -> c

instance Max 0 a a
else instance Max a 0 a
else instance
  ( Max a b c
  , Add 1 a aPlus1
  , Add 1 b bPlus1
  , Add 1 c cPlus1
  ) =>
  Max aPlus1 bPlus1 cPlus1

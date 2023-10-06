module Linear.Internal where

import Prelude

import Prim.RowList as RL

-- Record を使った実装？

newtype FunctionL a b = FunctionL (a -> b)

-- 遅れて評価される（ST 的な）
-- これを評価できるのは fin のみ（だと思う……）
-- これにより、letなどで一時的に束縛され返り値に使われない場合はスキップできる（はず）
newtype Linear :: Row Unique -> Type -> Type
newtype Linear r a = Linear (Unit -> a)

infixr 0 type FunctionL as -.

-- カインド Unique の型は forall で導入
-- 導入した Unique はすべて別物として扱われ、全く同一の場所で導入されたもののみ、同じものとなる
foreign import data Unique :: Type

-- | ある Row にある値が含まれているかを判定する
class Contains :: Row Unique -> Unique -> Constraint
class Contains r u

instance (ContainsList rl u, RL.RowToList r rl) => Contains r u

class ContainsList :: RL.RowList Unique -> Unique -> Constraint
class ContainsList rl u

instance ContainsList (RL.Cons l u rl) u
else instance (ContainsList rl u) => ContainsList (RL.Cons l u2 rl) u

-- | 与えられた Row に対して、指定された Unique が存在しない事を**保証**する
-- | make の引数に使う
-- | make 一つにつき u を 1 つ導入するので、u -> r である
-- | ↑が無いと何故かうまく動かないので追加
class NotElemProof :: Row Unique -> Unique -> Constraint
class NotElemProof r u | u -> r

-- | Unique が異なる事を保証する
-- | NotElemProof を用いる
-- | NotElemProof がない場合解決不能
class UniqueNotEq :: Unique -> Unique -> Constraint
class UniqueNotEq u1 u2

instance (NotElemProof r u2, Contains r u1) => UniqueNotEq u1 u2

-- | 与えられた Row に対して、指定された Unique が存在しない事を判定する
class NotElem :: Row Unique -> Unique -> Constraint
class NotElem r u

instance (NotElemList rl u, RL.RowToList r rl) => NotElem r u

-- | NotElem Constraint の導出に使う
class NotElemList :: RL.RowList Unique -> Unique -> Constraint
class NotElemList rl u

instance NotElemList RL.Nil u
instance (NotElemList rl u1, UniqueNotEq u1 u2) => NotElemList (RL.Cons l u2 rl) u1

-- | 与えられた Row 二つの共通部分が存在しない事を判定する
-- | Union Nub でいけない理由 : Unique が量化されているので型が確定しないため、NotElemProof をもとにして判定する
class Disjoint :: Row Unique -> Row Unique -> Constraint
class Disjoint r1 r2

instance (DisjointList r1 rl, RL.RowToList r2 rl) => Disjoint r1 r2

-- | Disjoint Constraint の導出に使う
class DisjointList :: Row Unique -> RL.RowList Unique -> Constraint
class DisjointList r rl

instance DisjointList r RL.Nil
instance (NotElem r u, DisjointList r rl) => DisjointList r (RL.Cons l u rl)

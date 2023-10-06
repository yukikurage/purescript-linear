module Linear.Internal
  ( FunctionL(..)
  , Linear(..)
  , type (-.)
  ) where

import Prelude

-- Record を使った実装？

newtype FunctionL a b = FunctionL (a -> b)

-- 遅れて評価される（ST 的な）
-- これを評価できるのは fin のみ（だと思う……）
-- これにより、letなどで一時的に束縛され返り値に使われない場合はスキップできる（はず）
newtype Linear :: Int -> Row Type -> Type -> Type
newtype Linear level r a = Linear (Unit -> a)

infixr 0 type FunctionL as -.

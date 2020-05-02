module LH where

data Operation = Insert | Update
  deriving Eq

{-@ predicate BtwnP Lo Hi V = Lo <= V && V <= Hi @-}

-- {-@ adjustBound ::
--       op:Operation ->
--       l:Nat ->
--       {u:Nat | l <= u} ->
--       _ ->
--       {v:Nat | if (isInsert op) then (BtwnP l (u+1) v) else BtwnP l u v}
-- @-}
adjustBound :: Operation -> Int -> Int -> Int -> Int
adjustBound op lower upper n
  | isInsert op = upper + 1
  | otherwise   = lower `max` (n `min` upper)

{-@ measure isInsert @-}
isInsert :: Operation -> Bool
isInsert Insert = True
isInsert _      = False

-- UNSAFE
-- f :: Int
-- f = adjustBound Insert (-100) (-50) (-70)
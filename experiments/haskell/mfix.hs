{-# LANGUAGE RecursiveDo #-}

data BTree
  = Empty
  | Fork Int BTree BTree
  deriving Show

sumTree t = do
  (t', _) <- sumTree' t 0
  return t'

sumTree' Empty acc = return (Empty, 0)
sumTree' (Fork x l r) acc = do
  (l', y) <- sumTree' l acc
  (r', z) <- sumTree' r acc
  return (Fork acc l' r', x + y + z)

tree = Fork 4 (Fork 3 Empty Empty) (Fork 5 Empty (Fork 1 Empty Empty))

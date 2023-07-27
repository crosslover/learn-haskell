import Data.List (sortBy)

-- file: ch03/ListADT.hs
data List a
  = Cons a (List a)
  | Nil
  deriving (Show)

fromList :: [a] -> List a
fromList (x : xs) = Cons x (fromList xs)
fromList [] = Nil

toList :: List a -> [a]
toList Nil = []
toList (Cons a xs) = a : toList xs

data Tree a
  = Node a (Tree a) (Tree a)
  | Empty
  deriving (Show)

data Tree' a
  = Node' a (Maybe (Tree' a)) (Maybe (Tree' a))
  deriving (Show)

simpleTree = Node' "parent" (Just (Node' "left child" Nothing Nothing)) (Just (Node' "right child" Nothing Nothing))

length' [] = 0
length' (_ : xs) = 1 + length' xs

avg xs = sum xs / length' xs

hw :: [a] -> [a]
hw [] = []
hw (a : xs) = a : hw xs ++ [a]

isHw [] = True
isHw (a : xs) = a == last xs && isHw (init xs)

sortByLen :: [[a]] -> [[a]]
sortByLen =
  sortBy
    ( \a b ->
        let r = length a - length b
         in case r of
              0 -> EQ
              _
                | r > 0 -> GT
                | r < 0 -> LT
    )

intersperse :: a -> [[a]] -> [a]
intersperse _ [] = []
intersperse sep (a : xs) = a ++ cc sep xs
  where
    cc sep' xs' = case xs' of
      [] -> []
      (a'' : xs'') -> sep' : a'' ++ cc sep' xs''

height :: Tree a -> Int
height Empty = 0
height (Node _ left right) = 1 + max (height left) (height right)

data Point = Point Double Double deriving (Show)

module MyLib where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

rev :: [a] -> [a]
rev [] = []
rev (x : xs) = rev xs ++ [x]

-- Напишите тесты к функции и функцию
--
-- Работает как zip, но если один список
-- длиннее, циклически переиспользует второй
--
-- > zipLong [1,2,3] "abc"
-- [(1,'a'),(2,'b'),(3,'c')]
--
-- > zipLong [1,2] "abcd"
-- [(1,'a'),(2,'b'),(1,'c'),(2,'d')]
--
-- > zipLong [] "abcd"
-- []
zipLong :: [a] -> [b] -> [(a,b)]
zipLong as bs = 
    if length as > length bs then
        zipLongSecond as bs bs
    else
        zipLongFirst as bs as
zipLongFirst (a:as) (b:bs) arr = (a, b) : zipLongFirst as bs arr
zipLongFirst [] (b:bs) (a:as) = (a, b) : zipLongFirst as bs (a:as)
zipLongFirst _ _ _ = []


zipLongSecond (a:as) (b:bs) arr = (a, b) : zipLongSecond as bs arr
zipLongSecond (a:as) [] (b:bs) = (a, b) : zipLongSecond as bs (b:bs)
zipLongSecond _ _ _ = []

-- Binary Search Tree
--
-- left < root <= right
data Tree a
  = Empty
  | Node
      { left :: Maybe (Tree a),
        value :: a,
        right :: Maybe (Tree a)
      }
  deriving (Eq, Show, Read)


empty :: Tree a
empty = Empty

leaf :: a -> Tree a
leaf a = Node Nothing a Nothing

traversal :: Tree a -> [a]
traversal Empty = []
traversal (Node ml v mr) =
  maybe [] traversal ml ++ [v] ++ maybe [] traversal mr


insert :: (Ord a) => a -> Tree a -> Tree a
insert v Empty = leaf v
insert v (Node ml root mr)
  | v < root = Node (Just $ maybe (leaf v) (insert v) ml) root mr
  | otherwise = Node ml root (Just $ maybe (leaf v) (insert v) mr)


-- Напишите тесты-свойства к функциям и сами функции
-- левого и правого поворота деревьев
-- (см. https://en.wikipedia.org/wiki/Red%E2%80%93black_tree)
rotateLeft :: Tree a -> Tree a
rotateLeft (Node ml v (Just (Node mrml rv mrmr))) = Node (Just (Node ml v mrml)) rv mrmr
rotateLeft tree = tree

rotateRight :: Tree a -> Tree a
rotateRight (Node (Just (Node mlml lv mlmr)) v mr) = Node mlml lv (Just (Node mlmr v mr))
rotateRight tree = tree

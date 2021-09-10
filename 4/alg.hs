{-# LANGUAGE GADTs #-}

-- бинарное дерево, без значений в ветвях, только в листьях
data Tree a where
  Leaf :: Ord a => a -> Tree a
  Branch :: Ord a => Tree a -> Tree a -> Tree a

-- проверка на то, есть ли элемент в листьях данного дерева, больший данного а
-- проверка идет только по правым поддеревьям, т.к. по построению там бОльшие элементы, чем в левом поддереве
superior :: Ord a => a -> Tree a -> Bool
superior a (Leaf b)            = a > b
superior a (Branch left right) = superior a right

-- вставка элемента в качестве листа дерева таким образом, чтобы потом с помощью in-order traversal получить сортированный список
insert :: Ord a => a -> Tree a -> Tree a
insert a (Leaf b) =
  if a < b
    then Branch (Leaf a) (Leaf b)
    else Branch (Leaf b) (Leaf a)
insert a (Branch left right) =
  if superior a left
    then Branch left (insert a right)
    else Branch (insert a left) right

-- построение дерева, последовательная вставка в дерево элементов списка
-- лучшая и худшая O(n * log n)
buildTree :: Ord a => [a] -> Tree a
buildTree []     = error "Used data structure does not store empty sets."
buildTree [a]    = Leaf a
buildTree (a:as) = insert a (buildTree as)

-- in-order обход
-- O(n)
traverseTree :: Ord a => Tree a -> [a]
traverseTree (Leaf a)            = [a]
traverseTree (Branch left right) = traverseTree left ++ traverseTree right

-- собственно сортировка
sort :: Ord a => [a] -> [a]
sort []  = []
sort lst = traverseTree . buildTree $ lst

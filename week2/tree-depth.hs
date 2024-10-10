data Tree = Leaf | Node Int Tree Tree

treeDepth :: Tree -> Int
treeDepth Leaf = 0
treeDepth (Node x left right) =
    1 + max (treeDepth left) (treeDepth right)
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------

module AATree
  ( AATree, -- type of AA search trees
    emptyTree, -- AATree a
    get, -- Ord a => a -> AATree a -> Maybe a
    insert, -- Ord a => a -> AATree a -> AATree a
    inorder, -- AATree a -> [a]
    remove, -- Ord a => a -> AATree a -> AATree a
    size, -- AATree a -> Int
    height, -- AATree a -> Int
    checkTree, -- Ord a => AATree a -> Bool
  )
where

--------------------------------------------------------------------------------

-- AA search trees

-- level left val right
data AATree a = EmptyTree | Node Int (AATree a) a (AATree a)
  deriving (Eq, Show, Read)

-- O(1)
emptyTree :: AATree a
emptyTree = EmptyTree

-- O(log n)
get :: (Ord a) => a -> AATree a -> Maybe a
get _ EmptyTree = Nothing
get x (Node _ left val right)
  | x == val = Just val
  | x < val = get x left
  | otherwise = get x right

-- You may find it helpful to define
-- O(1)
split :: AATree a -> AATree a
split (Node level left val (Node rlevel rleft rval rright@(Node rrlevel _ _ _)))
  | level == rrlevel = Node (rlevel + 1) (Node level left val rleft) rval rright
split t = t

-- O(1)
skew :: AATree a -> AATree a
skew (Node level (Node llevel lleft lval lright) val right)
  | level == llevel = Node llevel lleft lval (Node level lright val right)
skew t = t

-- and call these from insert.
-- O(log n)
insert :: (Ord a) => a -> AATree a -> AATree a
insert x EmptyTree = Node 1 EmptyTree x EmptyTree
insert x t@(Node lvl l v r)
  | x < v = split . skew $ Node lvl (insert x l) v r -- if x is less than the node go to the left
  | x > v = split . skew $ Node lvl l v (insert x r) -- if x is bigger than the node go to the right
  | otherwise = t -- x == v, do nothing

-- O(n)
inorder :: AATree a -> [a]
inorder t = inorder' t []
  where
    inorder' EmptyTree acc = acc
    inorder' (Node _ l v r) acc = inorder' l (v : inorder' r acc)

-- O(n)
size :: AATree a -> Int
size EmptyTree = 0
size (Node _ l _ r) = size l + 1 + size r

-- O(n)
height :: AATree a -> Int
height EmptyTree = 0
height (Node _ l _ r) = 1 + max (height l) (height r)

--------------------------------------------------------------------------------
-- Optional function

-- should be O(log n)
remove :: (Ord a) => a -> AATree a -> AATree a
remove = error "remove not implemented"

--------------------------------------------------------------------------------
-- Check that an AA tree is ordered and obeys the AA invariants

-- O(n)
checkTree :: (Ord a) => AATree a -> Bool
checkTree root =
  isSorted (inorder root)
    && all checkLevels (nodes root)
  where
    nodes x
      | isEmpty x = []
      | otherwise = x : nodes (leftSub x) ++ nodes (rightSub x)

-- True if the given list is ordered
-- O(n)
isSorted :: (Ord a) => [a] -> Bool
isSorted [] = True
isSorted [_] = True
isSorted (x : y : zs) = x < y && isSorted (y : zs)

-- Check if the invariant is true for a single AA node
-- O(1)
checkLevels :: AATree a -> Bool
checkLevels EmptyTree = True
checkLevels (Node lvl l _ r) = leftChildOK && rightChildOK && rightGrandchildOK
  where
    leftChildOK = levelOf l < lvl -- left child lvl < lvl
    rightChildOK = levelOf r == lvl || levelOf r == lvl - 1 -- right child lvl = lvl || lvl - 1
    rightGrandchildOK =
      case r of -- right level <= parent
        Node _ _ _ rr -> levelOf rr < lvl -- right-right level < parent
        _ -> True

    levelOf EmptyTree = 0
    levelOf (Node nlvl _ _ _) = nlvl

-- O(1)
isEmpty :: AATree a -> Bool
isEmpty EmptyTree = True
isEmpty (Node _ _ _ _) = False

-- O(1)
leftSub :: AATree a -> AATree a
leftSub EmptyTree = EmptyTree
leftSub (Node _ lt _ _) = lt

-- O(1)
rightSub :: AATree a -> AATree a
rightSub EmptyTree = EmptyTree
rightSub (Node _ _ _ rt) = rt

--------------------------------------------------------------------------------

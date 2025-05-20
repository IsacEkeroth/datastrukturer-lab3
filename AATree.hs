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

emptyTree :: AATree a
emptyTree = EmptyTree

get :: (Ord a) => a -> AATree a -> Maybe a
get _ EmptyTree = Nothing
get x (Node _ left val right)
  | x == val = Just val
  | x < val = get x left
  | otherwise = get x right

-- You may find it helpful to define
split :: AATree a -> AATree a
split EmptyTree = EmptyTree
split (Node level left val (Node rlevel rleft rval rright@(Node rrlevel _ _ _)))
  | level == rrlevel = Node (rlevel + 1) (Node level left val rleft) rval rright
split t = t

skew :: AATree a -> AATree a
skew EmptyTree = EmptyTree
skew (Node level (Node llevel lleft lval lright) val right)
  | level == llevel = Node llevel lleft lval (Node level lright val right)
skew t = t

-- and call these from insert.
insert :: (Ord a) => a -> AATree a -> AATree a
insert x EmptyTree = Node 1 EmptyTree x EmptyTree
insert x t@(Node lvl l v r)
  | x < v = split . skew $ Node lvl (insert x l) v r
  | x > v = split . skew $ Node lvl l v (insert x r)
  | otherwise = t -- x == v, do nothing

inorder :: AATree a -> [a]
inorder EmptyTree = []
inorder (Node 1 _ v _) = [v]
inorder (Node _ _ _ EmptyTree) = []
inorder (Node lvl l v r@(Node rlvl rl rv rr))
  | lvl == rlvl = inorder l ++ [v] ++ inorder rl ++ [rv] ++ inorder rr
  | otherwise = inorder l ++ [v] ++ inorder r

size :: AATree a -> Int
size EmptyTree = 0
size (Node _ l _ r) = size l + 1 + size r

height :: AATree a -> Int
height EmptyTree = 0
height (Node lv _ _ _) = lv

--------------------------------------------------------------------------------
-- Optional function

remove :: (Ord a) => a -> AATree a -> AATree a
remove = error "remove not implemented"

--------------------------------------------------------------------------------
-- Check that an AA tree is ordered and obeys the AA invariants

checkTree :: (Ord a) => AATree a -> Bool
checkTree root =
  isSorted (inorder root)
    && all checkLevels (nodes root)
  where
    nodes x
      | isEmpty x = []
      | otherwise = x : nodes (leftSub x) ++ nodes (rightSub x)

-- True if the given list is ordered
isSorted :: (Ord a) => [a] -> Bool
isSorted = error "isSorted not implemented"

-- Check if the invariant is true for a single AA node
-- You may want to write this as a conjunction e.g.
--   checkLevels node =
--     leftChildOK node &&
--     rightChildOK node &&
--     rightGrandchildOK node
-- where each conjunct checks one aspect of the invariant
checkLevels :: AATree a -> Bool
checkLevels = error "checkLevels not implemented"

isEmpty :: AATree a -> Bool
isEmpty EmptyTree = True
isEmpty (Node _ _ _ _) = False

leftSub :: AATree a -> AATree a
leftSub EmptyTree = EmptyTree
leftSub (Node _ lt _ _) = lt

rightSub :: AATree a -> AATree a
rightSub EmptyTree = EmptyTree
rightSub (Node _ _ _ rt) = rt

--------------------------------------------------------------------------------

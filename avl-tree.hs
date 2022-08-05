module Main where

type Val = Int

data Tree
  = Nil
  | Node Tree Val Tree
  deriving (Eq, Show) -- left subtree, node content, right subtree

before :: Val -> Val -> Bool
before = (<)

preOrder :: Tree -> [Val]
preOrder Nil                   = []
preOrder (Node left val right) = val : ((preOrder left) ++ (preOrder right))

inOrder :: Tree -> [Val]
inOrder Nil                   = []
inOrder (Node left val right) = (inOrder left) ++ (val : inOrder right)

postOrder :: Tree -> [Val]
postOrder Nil = []
postOrder (Node left val right) =
  ((postOrder left) ++ (postOrder right)) ++ [val]

insert :: Val -> Tree -> Tree
insert a Nil = Node Nil a Nil
insert a (Node left val right)
  | a `before` val = Node (insert a left) val right
  | otherwise = Node left val (insert a right)

rotRight :: Tree -> Tree
rotRight Nil                                = Nil
rotRight node@(Node Nil _ _)                = node -- Rotation is not possible, so it's ignored
rotRight (Node (Node ll lval lr) val right) = Node ll lval (Node lr val right)

rotLeft :: Tree -> Tree
rotLeft Nil                               = Nil
rotLeft node@(Node _ _ Nil)               = node -- Rotation is not possible, so it's ignored
rotLeft (Node left val (Node rl rval rr)) = Node (Node left val rl) rval rr

height :: Tree -> Int
height Nil                 = 0
height (Node left _ right) = 1 + max (height left) (height right)

balanceFactor :: Tree -> Int
balanceFactor Nil                 = 0
balanceFactor (Node left _ right) = height right - height left

worstBf :: Tree -> Int
worstBf Nil = 0
worstBf node@(Node left _ right)
  | abs (balanceFactor node) > abs (worstBf left) &&
      abs (balanceFactor node) > abs (worstBf right) = balanceFactor node
  | abs (worstBf left) > abs (worstBf right) = worstBf left
  | otherwise = worstBf right

avlBalance :: Tree -> Tree
avlBalance Nil = Nil
avlBalance tree@(Node left val right)
  | futureBF > 1 = avlBalance $ rotLeft futureNode
  | futureBF < -1 = avlBalance $ rotRight futureNode
  | otherwise = futureNode
  where
    futureLeft = avlBalance left
    futureRight = avlBalance right
    futureNode = Node (futureLeft) val (futureRight)
    futureBF = balanceFactor futureNode

avlTest :: Int -> Tree
avlTest n = avlTest' n Nil
  where
    avlTest' i tree
      | i == 1 = insert i tree
      | otherwise = avlTest' (i - 1) (insert i tree)

main = undefined -- NOTE: This is meant to be played with in ghci

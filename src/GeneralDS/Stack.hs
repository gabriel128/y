module GeneralDS.Stack where

newtype Stack a = Stack [a]
  deriving (Show, Eq)

new :: Stack a
new = Stack []

pop :: Stack a -> (Maybe a, Stack a)
pop (Stack (x : xs)) = (Just x, Stack xs)
pop stack = (Nothing, stack)

push :: a -> Stack a -> Stack a
push x (Stack xs) = Stack (x : xs)

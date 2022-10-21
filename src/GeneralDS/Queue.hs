module GeneralDS.Queue where

data Queue a = Queue [a] [a]
  deriving (Show, Eq)

new :: Queue a
new = Queue [] []

len :: Queue a -> Int
len (Queue xls xrs) = length xls + length xrs

toList :: Queue a -> [a]
toList (Queue xls rls) = xls ++ reverse rls

enqueue :: a -> Queue a -> Queue a
enqueue x (Queue xls xrs) = Queue xls (x : xrs)

dequeue :: Queue a -> (Maybe a, Queue a)
dequeue queue@(Queue [] []) = (Nothing, queue)
dequeue (Queue [] xrs) = dequeue $ Queue (reverse xrs) []
dequeue (Queue (x : xls) xrs) = (Just x, Queue xls xrs)

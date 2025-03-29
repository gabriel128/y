module GeneralDS.Queue where

-- | Implements an amortized constant queue based on Okazaki's batched queue
data Queue a = Queue Word [a] Word [a]
  deriving (Show, Eq)

new :: Queue a
new = Queue 0 [] 0 []

len :: Queue a -> Word
len (Queue llen _ rlen _) = llen + rlen

toList :: Queue a -> [a]
toList (Queue _ xls _ xrs) = xls ++ reverse xrs

enqueue :: a -> Queue a -> Queue a
enqueue x (Queue llen xls rlen xrs) = Queue llen xls (rlen + 1) (x : xrs)

dequeue :: Queue a -> (Maybe a, Queue a)
dequeue queue@(Queue _ [] _ []) = (Nothing, queue)
dequeue (Queue _ [] rlen xrs) = dequeue $ Queue rlen (reverse xrs) 0 []
dequeue (Queue llen (x : xls) rlen xrs) = (Just x, Queue (llen - 1) xls rlen xrs)

instance Functor Queue where
  fmap f (Queue llen xls rlen xrs) = Queue llen (fmap f xls) rlen (fmap f xrs)

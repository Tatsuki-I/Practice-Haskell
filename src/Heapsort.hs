module Heapsort where

data Heap a = Leaf (Maybe a) 
            | Node (Heap a) a (Heap a)
              deriving ( Show
                       , Eq)

makeHeap :: (Foldable t, Ord a) => t a -> Heap a
makeHeap =  foldr push (Leaf Nothing) 

push                  :: Ord a => a -> Heap a -> Heap a
push x h@(Node l e r) |  x < e     = if r == Leaf Nothing
                                        then Node l
                                                  e
                                                  (push x r)
                                        else Node (push x l)
                                                  e
                                                  r
                      |  otherwise = Node h 
                                          x
                                          (Leaf Nothing)

push x (Leaf e)       =  case e of 
                              Just n -> Node (Leaf (Just (min n x)))
                                             (max n x)
                                             (Leaf Nothing)
                              Nothing        -> Leaf (Just x)



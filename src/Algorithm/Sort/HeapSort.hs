module Algorithm.Sort.HeapSort where

import Data.Maybe
import Data.List

data Heap a = Leaf (Maybe a)
            | Node (Heap a) a (Heap a)
              deriving ( Show
                       , Eq)

mkHeap :: (Foldable t, Ord a) => t a -> Heap a
mkHeap =  foldr push (Leaf Nothing)

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

toList (Leaf e)     =  case e of
                            Just n  -> [n]
                            Nothing -> []
toList (Node l e r) =  toList l  ++ e : toList r

root (Node _ e _) = Just e
root (Leaf l)     = l

heapsort []  = []
heapsort [x] = [x]
heapsort xs  = rt : heapsort (delete rt xs)
               where rt = (fromJust . root . mkHeap) xs

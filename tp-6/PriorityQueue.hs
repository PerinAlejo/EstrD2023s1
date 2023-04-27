module PriorityQueue 
    (PriorityQueue, emptyPQ, isEmptyPQ, insertPQ, findMinPQ, deleteMinPQ)
where

data PriorityQueue a = PQ [a]
{-
INV. REP.: Para PQ l :
    -l Esta ordenada de menor a mayor orden de prioridad
-}

emptyPQ :: PriorityQueue a                                      --O(1)
emptyPQ = PQ []

isEmptyPQ :: PriorityQueue a -> Bool                            --O(1)
isEmptyPQ (PQ l) = null l

insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a    --O(n)
insertPQ x (PQ l) = PQ (insertar x l)

insertar :: Ord a => a -> [a] -> [a]                            --O(n)
insertar e   []   = [e]                             
insertar e (x:xs) = if e > x 
                    then x : insertar e xs
                    else e : x : xs

--Precondición: parcial en caso de priority queue vacía.
findMinPQ :: Ord a => PriorityQueue a -> a                      --O(1)
findMinPQ (PQ l) = head l

--Precondición: parcial en caso de priority queue vacía.
deleteMinPQ :: Ord a => PriorityQueue a -> PriorityQueue a      --O(1)
deleteMinPQ (PQ l) = PQ (tail l)

heapSort :: Ord a => [a] -> [a]
heapSort xs = listaPQ (priorityQueueDe xs)

priorityQueueDe :: Ord a => [a] -> PriorityQueue a
priorityQueueDe   []   = emptyPQ
priorityQueueDe (x:xs) = insertPQ x (priorityQueueDe xs)

listaPQ :: PriorityQueue a -> [a]
listaPQ (PQ l) = l
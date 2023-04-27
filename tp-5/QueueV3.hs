module QueueV2 
    (Queue,emptyQ,isEmptyQ,enqueue,firstQ,dequeue)
where

data Queue a = Q [a] [a]
{-
INV. REP.: para Q fs bs
     -Si fs se encuentra vacía, entonces la cola se encuentra vacía.
     -Quitaremos elementos a través de fs
     -Agregaremos a través de bs
-}

emptyQ :: Queue a
emptyQ = Q [] []                                 --Constante
 
isEmptyQ :: Queue a -> Bool                      --Constante
isEmptyQ (Q fs _ ) = null fs

enqueue :: a -> Queue a -> Queue a               --Constante
enqueue x (Q fs bs) = (Q fs (x:bs))

firstQ :: Queue a -> a                           --Constante
firstQ (Q fs _) = head fs

dequeue :: Queue a -> Queue a                    --Constante
dequeue (Q fs bs) = (Q (tail fs) bs) 

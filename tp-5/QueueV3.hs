module QueueV3 
    (Queue,emptyQ,isEmptyQ,enqueue,firstQ,dequeue)
where

data Queue a = Q [a] [a]
{-
INV. REP.: para Q fs bs
     -Si fs se encuentra vacía, entonces la cola se encuentra vacía.
-}

emptyQ :: Queue a
emptyQ = Q [] []                                 --Constante
 
isEmptyQ :: Queue a -> Bool                      --Constante
isEmptyQ (Q fs _ ) = null fs

enqueue :: a -> Queue a -> Queue a               --Constante
enqueue x (Q fs bs) = if null fs  
                      then (Q (x:fs) bs)
                      else (Q fs (x:bs))
    
firstQ :: Queue a -> a                           --Constante
firstQ (Q fs _) = head fs

dequeue :: Queue a -> Queue a                    --Constante amortizada, ya que se realiza O(n) pero despues se realizan n operaciones O(1)
dequeue (Q fs bs) = if tieneUnicoElem fs
                    then (Q (reversaLineal bs) [])
                    else (Q (tail fs) bs)

reversaLineal :: [a] -> [a] -> [a]
reversaLineal   []   l = l
reversaLineal (x:xs) l = reversaLineal xs (x:l)

tieneUnicoElem :: [a] -> Bool
tieneUnicoElem [x] = True
tieneUnicoElem  _  = False

 
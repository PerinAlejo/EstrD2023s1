module QueueV2 
    (Queue,emptyQ,isEmptyQ,enqueue,firstQ,dequeue)
where

data Queue a = Q [a]

{-
INV. REP:
    Para Q l
        -El primer elemento que se agrega a l es el primero en salir.
        -El elemento ingresado a l debe colarse al inicio de la lista
        -El elemento a sacar debe descolarse por detras de la lista
-}

emptyQ :: Queue a                           --Constante
emptyQ = (Q [])                             

isEmptyQ :: Queue a -> Bool                 --Constante
isEmptyQ q  = null (listaQ q)               

listaQ :: Queue a -> [a]                    --Constante
listaQ (Q l) = l                            

enqueue :: a -> Queue a -> Queue a          --Constante
enqueue e q = (Q (e:(listaQ q)))  

--PREC: La lista no debe ser vacia
firstQ :: Queue a -> a                      --Lineal
firstQ q = ultimoLQ (listaQ q)

ultimoLQ :: [a] -> a                        --Lineal 
ultimoLQ   []   = error "No hay elementos"
ultimoLQ  [x]   = x
ultimoLQ (x:xs) = ultimoLQ xs

dequeue :: Queue a -> Queue a               --Lineal
dequeue q = (Q (sinElUltimoL (listaQ q)))

sinElUltimoL :: [a] -> [a] 
sinElUltimoL   []   = []
sinElUltimoL  [x]   = [] 
sinElUltimoL (x:xs) = x : sinElUltimoL xs
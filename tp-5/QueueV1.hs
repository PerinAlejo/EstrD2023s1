module QueueV1 
    (Queue, )
where

data Queue a = Q [a]

{-
INV. REP:
    Para Q l
        -El primer elemento que se agrega a l es el primero en salir.
        -El elemento ingresado a l debe colarse al final de la lista
        -El elemento a sacar debe descolarse por delante de la lista
-}

emptyQ :: Queue a                           --Constante
emptyQ = (Q [])                             

isEmptyQ :: Queue a -> Bool                 --Constante
isEmptyQ q  = null (listaQ q)               

listaQ :: Queue a -> [a]                    --Constante
listaQ (Q l) = l                            

enqueue :: a -> Queue a -> Queue a          --Lineal
enqueue e q = (Q ((listaQ q) ++ [e]))  

firstQ :: Queue a -> a                      --Constante
firstQ q = if isEmptyQ q                  
            then error "No hay elementos"
            else head (listaQ q)

dequeue :: Queue a -> Queue a               --Constante
dequeue q = if isEmptyQ q
            then emptyQ
            else (Q (tail(listaQ q)))
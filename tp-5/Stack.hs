module Stack
    (Stack,emptySt,isEmptySt,push,top,pop,lenSt)
where

data Stack a = S [a] Int          
{-
INV. REP: 
    Para S l c
        -El ultimo elemento en entrar a l es el primero en salir.
        -El valor de c representa la cantidad de elementos en l
-}

emptySt :: Stack a 
emptySt = (S [] 0)

isEmptySt :: Stack a -> Bool 
isEmptySt (S l _) = null l 

push :: a -> Stack a -> Stack a
push e (S l c) = (S (e:l) (c+1)) 

top :: Stack a -> a
top s = if isEmptySt s 
        then error "No hay elementos"
        else head (listaSt s) 

pop :: Stack a -> Stack a 
pop s = if isEmptySt s
        then emptySt
        else (S (tail (listaSt s)) (lenSt s - 1))

listaSt :: Stack a -> [a]
listaSt (S l _) = l 

lenSt :: Stack a -> Int 
lenSt (S _ c) = c
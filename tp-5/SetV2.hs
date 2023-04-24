module SetV2
    (Set,sizeS,setToList,emptyS,belongs,addS,removeS,unionS)
where

data Set a = S [a] 

s1= S [1,1,2,2,3,3,4,4,5,5,6,6,7,7]

sizeS :: Eq a => Set a -> Int                    --Lineal
sizeS s = length(setToList s)

setToList :: Eq a => Set a -> [a]                --Lineal
setToList (S l) = sinRepetidos l

sinRepetidos :: Eq a => [a] -> [a]               --Lineal + Constante = Lineal
sinRepetidos [] = []
sinRepetidos (x:xs) =
    if elem x xs              --Lineal
        then sinRepetidos xs       --Constante
        else x : sinRepetidos xs   --Constante

emptyS :: Set a                                  --Constante
emptyS = (S [])

belongs :: Eq a => a -> Set a -> Bool            --Lineal
belongs e (S l) = elem e l 

addS :: Eq a => a -> Set a -> Set a              --Constante
addS e (S l) = (S (e:l))
            
removeS :: Eq a => a -> Set a -> Set a           --Lineal
removeS e (S l) = if elem e l
                then (S (quitarTodos e l))
                else (S l)

quitarTodos :: Eq a => a -> [a] -> [a]           --Lineal
quitarTodos e   []   = []
quitarTodos e (x:xs) = if e == x
                    then quitarTodos e xs
                    else x : quitarTodos e xs

unionS :: Eq a => Set a -> Set a -> Set a        --Lineal   
unionS s1 s2 = (S ((listaS s1) ++ (listaS s2))) 

listaS :: Set a -> [a]                           --Constante
listaS (S l) = l
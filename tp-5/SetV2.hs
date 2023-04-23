module SetV2
    (Set,sizeS,setToList,emptyS,belongs,addS,removeS,unionS )
where

data Set a = S [a] Int
{-
INV. REP:
    -Para S l c:
        c es la cantidad de elementos en l 
-}

sizeS :: Eq a => Set a -> Int                    --Constante
sizeS (S _ c) = c

setToList :: Eq a => Set a -> [a]                --Lineal
setToList (S l _) = sinRepetidos l

sinRepetidos :: Eq a => [a] -> [a]  --Lineal + Constante = Lineal
sinRepetidos [] = []
sinRepetidos (x:xs) =
    if pertenece x xs              --Lineal
        then sinRepetidos xs       --Constante
        else x : sinRepetidos xs   --Constante

emptyS :: Set a                                  --Constante
emptyS = (S [] 0)

belongs :: Eq a => a -> Set a -> Bool            --Lineal
belongs e (S l _) = elem e l 

addS :: Eq a => a -> Set a -> Set a              --Constante
addS e (S l c) = (S (e:l) c)
            
removeS :: Eq a => a -> Set a -> Set a           --Lineal
removeS e s = if belongs e s 
                then (S (quitar e (setToList s)) (sizeS s - 1))
                else s
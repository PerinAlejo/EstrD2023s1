module SetV1
    (Set,sizeS,setToList,emptyS,belongs,addS,removeS,unionS )
where

data Set a = S [a] Int
{-
INV. REP:
    -Para S l c:
        l No puede tener repetidos
        c es la cantidad de elementos en l 
-}

sizeS :: Eq a => Set a -> Int                    --Constante
sizeS (S _ c) = c

setToList :: Eq a => Set a -> [a]                --Constante
setToList (S l _) = l

emptyS :: Set a                                  --Constante
emptyS = (S [] 0)

belongs :: Eq a => a -> Set a -> Bool            --Lineal
belongs e (S l _) = elem e l 

addS :: Eq a => a -> Set a -> Set a              --Lineal
addS e s = if belongs e s
            then s
            else (S (e : setToList s) (1 + sizeS s))

removeS :: Eq a => a -> Set a -> Set a           --Lineal
removeS e s = if belongs e s 
                then (S (quitar e (setToList s)) (sizeS s - 1))
                else s

quitar :: Eq a => a -> [a] -> [a] --Lineal
quitar e   []   = []
quitar e (x:xs) = if e == x
                    then xs
                    else x : quitar e xs 

unionS :: Eq a => Set a -> Set a -> Set a  --Lineal
unionS s1 s2 = (S (agregarSinRep (setToList s1) (setToList s2)) (length (agregarSinRep (setToList s1)(setToList s2))))

agregarSinRep :: Eq a => [a] -> [a] -> [a]  -- Lineal
agregarSinRep   []   ys = ys
agregarSinRep (x:xs) ys = if elem x ys
                            then agregarSinRep xs ys
                            else x : agregarSinRep xs ys
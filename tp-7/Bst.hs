module Bst
    (Tree,belongsBST)
where

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
        deriving Show
{- 
INV. REP.: El arbol debe ser un BST y no debe tener elementos repetidos
-}

bst = (NodeT 20
        (NodeT 10 
            (NodeT 5 EmptyT EmptyT) 
            (NodeT 15 EmptyT EmptyT)) 
        (NodeT 30 
            (NodeT 25 EmptyT EmptyT) 
            (NodeT 35 EmptyT EmptyT))) 



belongsBST :: Ord a => a -> Tree a -> Bool                 --O(log n) Si el arbol esta balanceado
belongsBST x      EmptyT     = False
belongsBST x (NodeT y ti td) = if x == y 
                                then True
                                else if x < y
                                     then belongsBST x ti
                                     else belongsBST x td

------------------------------------------------------------------
--Propósito: dado un BST inserta un elemento en el árbol.
insertBST :: Ord a => a -> Tree a -> Tree a                --O(log n) Si el arbol esta balanceado
insertBST x      EmptyT     = NodeT x EmptyT EmptyT
insertBST x (NodeT y ti td) = if x == y
                              then (NodeT x ti td) 
                              else if x < y
                                   then NodeT y (insertBST x ti) td
                                   else NodeT y ti (insertBST x td)  

------------------------------------------------------------------
--Propósito: dado un BST borra un elemento en el árbol.
deleteBST :: Ord a => a -> Tree a -> Tree a                --O(log n)
deleteBST x      EmptyT     = EmptyT
deleteBST x (NodeT y ti td) = 
    if x == y then rearmarBST ti td   
     else if x < y then NodeT y (deleteBST x ti) td
        else NodeT y ti (deleteBST x td)

rearmarBST :: Ord a => Tree a -> Tree a -> Tree a          --O(log n)
rearmarBST EmptyT td = td
rearmarBST   ti   td = let (maxBst, ti') = splitMaxBST ti
                        in NodeT maxBst ti' td

------------------------------------------------------------------
--Propósito: dado un BST devuelve un par con el mínimo elemento y el árbol sin el mismo.
--PREC: El arbol no puede ser vacio
splitMinBST :: Ord a => Tree a -> (a,Tree a)               --O(log n)
splitMinBST (NodeT x EmptyT td) = (x, td)
splitMinBST   (NodeT x ti td)   = let (minBst, delMinBst) = splitMinBST ti in 
                                  (minBst, (NodeT x delMinBst td)) 

------------------------------------------------------------------
--Propósito: dado un BST devuelve un par con el máximo elemento y el árbol sin el mismo.
--PREC: El arbol no puede ser vacio
splitMaxBST :: Ord a => Tree a-> (a,Tree a)                --O(log n)
splitMaxBST (NodeT x ti EmptyT) = (x, ti)
splitMaxBST   (NodeT x ti td)   = let (maxBst, delMaxBst) = splitMaxBST td in 
                                  (maxBst, (NodeT x ti delMaxBst))

------------------------------------------------------------------
--Propósito: indica si el árbol cumple con los invariantes de BST.
esBST :: Ord a => Tree a -> Bool
esBST     EmptyT      = True
esBST (NodeT x ti td) = esMayor x (root ti) && esMenor x (root td) && esBST ti && esBST td

esMayor :: Ord a => a -> Maybe a -> Bool 
esMayor _ Nothing  = True
esMayor x (Just y) = x > y 

esMenor :: Ord a => a -> Maybe a -> Bool 
esMenor _ Nothing  = True
esMenor x (Just y) = x < y

--PREC: El arbol no puede ser vacio
root :: Tree a -> Maybe a
root     EmptyT    = Nothing
root (NodeT x _ _) = Just x

fromJust :: Maybe v -> v
fromJust Nothing  = error "No existe el valor"
fromJust (Just v) = v

------------------------------------------------------------------
--Propósito: dado un BST y un elemento, devuelve el máximo elemento que sea menor al elemento dado
--Costo: Como es O(log n + log n) es igual a O(log n)
elMaximoMenorA :: Ord a=> a -> Tree a -> Maybe a  
elMaximoMenorA x      EmptyT     = Nothing
elMaximoMenorA x (NodeT y ti td) = if x > y && mayorQue (elMinimoBST td) x  --O(log n)  
                                    then Just y  
                                    else if x > y 
                                         then elMaximoMenorA x td  --O(log n)
                                         else elMaximoMenorA x ti  --O(log n)

--Costo: O(1)
mayorQue :: Ord a => Maybe a -> a -> Bool
mayorQue Nothing _  = True
mayorQue (Just x) y = x > y

--Costo: O(log n)
elMinimoBST :: Ord a => Tree a -> Maybe a        
elMinimoBST       EmptyT       = Nothing
elMinimoBST (NodeT x EmptyT _) = Just x
elMinimoBST   (NodeT x ti td)  = elMinimoBST ti

------------------------------------------------------------------
--Propósito: dado un BST y un elemento, devuelve el mínimo elemento que sea mayor al elemento dado
--Costo: Como es O(log n + log n) es igual a O(log n) 
elMinimoMayorA :: Ord a => a -> Tree a -> Maybe a
elMinimoMayorA x      EmptyT     = Nothing
elMinimoMayorA x (NodeT y ti td) = if y > x && menorQue (elMaximoBST ti) x  --O(log n)
                                    then Just y
                                    else if y > x 
                                         then elMinimoMayorA x ti  --O(log n)
                                         else elMinimoMayorA x td  --O(log n)
--Costo: O(log n)
elMaximoBST :: Ord a => Tree a -> Maybe a        
elMaximoBST       EmptyT       = Nothing
elMaximoBST (NodeT x _ EmptyT) = Just x
elMaximoBST   (NodeT x ti td)  = elMaximoBST td

--Costo: O(1)
menorQue :: Ord a => Maybe a -> a -> Bool
menorQue Nothing _  = True
menorQue (Just x) y = x < y

------------------------------------------------------------------
--Propósito: indica si el árbol está balanceado. Un árbol está balanceado cuando para cada nodo la diferencia de alturas entre el subarbol izquierdo y el derecho es menor o igual a 1.
balanceado :: Tree a -> Bool
balanceado     EmptyT      = True
balanceado (NodeT _ ti td) = let hs = ((heightT ti),(heightT td)) in
        (maxDelPar hs - minDelPar hs) < 2 && balanceado ti && balanceado td

heightT :: Tree a -> Int
heightT       EmptyT    = 0
heightT (NodeT e a1 a2) = 1 + max (heightT a1) (heightT a2)

maxDelPar :: (Int,Int) -> Int
maxDelPar (n, m) = if (n > m)
                       then n
                       else m

minDelPar :: (Int,Int) -> Int
minDelPar (n, m) = if (n < m)
                       then n
                       else m


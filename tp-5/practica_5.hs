import SetV1
--1. Cálculo de costos


head' :: [a] -> a       --Constante porque revisa un elemento y lo devuelve
head' (x:xs) = x

------------------------------------------------------------------------------------
sumar :: Int -> Int                              --Constante porque suma una vez sea el elemento que sea
sumar x = x + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1  

------------------------------------------------------------------------------------
factorial :: Int -> Int            --Lineal ya que depende del elemento es la veces que va a repetir la
factorial 0 = 1                    --operacion
factorial n = n * factorial (n-1)

------------------------------------------------------------------------------------
longitud :: [a] -> Int             --Lineal, suma uno por casda elemento de la lista
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

------------------------------------------------------------------------------------
factoriales :: [Int] -> [Int]                     --Cuadratica ya que por cada elemento de la lista 
factoriales [] = []                               --aplica una funcion lineal
factoriales (x:xs) = factorial x : factoriales xs

------------------------------------------------------------------------------------
pertenece :: Eq a => a -> [a] -> Bool         --Lineal ya que por cada elemento de la lista verifica si
pertenece n [] = False                        --es igual al elemento dado
pertenece n (x:xs) = n == x || pertenece n xs

------------------------------------------------------------------------------------
sinRepetidos' :: Eq a => [a] -> [a]  --Lineal + Constante = Lineal
sinRepetidos' [] = []
sinRepetidos' (x:xs) =
    if pertenece x xs              --Lineal
        then sinRepetidos' xs       --Constante
        else x : sinRepetidos' xs   --Constante

------------------------------------------------------------------------------------
-- equivalente a (++)
append :: [a] -> [a] -> [a]          --Lineal porque hace una operacion constante por cada elemento de
append [] ys = ys                    --la lista
append (x:xs) ys = x : append xs ys

------------------------------------------------------------------------------------
concatenar :: [String] -> String       --Cuadratica porque por cada elemento hace una operacion lineal  
concatenar [] = []
concatenar (x:xs) = x ++ concatenar xs

------------------------------------------------------------------------------------
takeN :: Int -> [a] -> [a]           --Lineal porque hace una operacion constante segun el numero dado
takeN 0 xs = []                      --o los elementos de la lista
takeN n [] = []
takeN n (x:xs) = x : takeN (n-1) xs

------------------------------------------------------------------------------------
dropN :: Int -> [a] -> [a]       --lineal porque va restando uno hasta que el numero dado sea 0
dropN 0 xs = xs
dropN n [] = []
dropN n (x:xs) = dropN (n-1) xs

------------------------------------------------------------------------------------
partir :: Int -> [a] -> ([a], [a])     --Lineal ya que lineal + lineal = lineal
partir n xs = (takeN n xs, dropN n xs)

------------------------------------------------------------------------------------
minimo :: Ord a => [a] -> a       --Lineal porque hace una operacion constante por cada elemento de la
minimo [x] = x                    --lista
minimo (x:xs) = min x (minimo xs)

------------------------------------------------------------------------------------
sacar :: Eq a => a -> [a] -> [a]  --Lineal ya que saca elementos segun n o la cantidad de elementos
sacar n [] = []                   --de la lista
sacar n (x:xs) =
    if n == x               
        then xs
        else x : sacar n xs 

------------------------------------------------------------------------------------
ordenar :: Ord a => [a] -> [a]      --Cuadratica
ordenar [] = []
orderar xs =
    let m = minimo xs               --Lineal
        in m : ordenar (sacar m xs) -- (sacar m xs) = lineal*lineal = Cuadratica 
                                    -- (m : ordenar (sacar m xs)) == lineal + constante + cuadratica = cu

-- ======================================================================================
-- ======================================================================================    
-- 2. Set (conjunto)

set1 = addS 4(addS 3 (addS 2(addS 1 (emptyS))))
set2 = addS 5(addS 6 (addS 2(addS 1 (emptyS))))
set3 = addS 4(addS 3 (addS 7(addS 8 (emptyS))))

losQuePertenecen :: Eq a => [a] -> Set a -> [a]
losQuePertenecen   []   s = []
losQuePertenecen (x:xs) s = if belongs x s
                            then x : losQuePertenecen xs s
                            else losQuePertenecen xs s

------------------------------------------------------------------------------------
sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos xs = setToList(agregarASet xs)

agregarASet :: Eq a => [a] -> Set a
agregarASet   []   = emptyS
agregarASet (x:xs) = addS x (agregarASet xs)

------------------------------------------------------------------------------------

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)

unirTodos :: Eq a => Tree (Set a) -> Set a
unirTodos      EmptyT     = emptyS
unirTodos (NodeT s t1 t2) = unionS s (unionS (unirTodos t1) (unirTodos t2))

treeSet = NodeT set1 (NodeT set2 EmptyT EmptyT) (NodeT set3 EmptyT EmptyT)

-- ======================================================================================
-- ====================================================================================== 
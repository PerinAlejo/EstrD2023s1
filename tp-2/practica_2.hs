--1. Recursión sobre listas

sumatoria :: [Int] -> Int
sumatoria   []   = 0
sumatoria (x:xs) = x + sumatoria xs

longitud :: [a] -> Int
longitud   []   = 0
longitud (x:xs) = 1 + longitud xs

sucesores :: [Int] -> [Int]
sucesores   []   = []
sucesores (x:xs) = x + 1 : sucesores xs    

--Preguntar
conjuncion :: [Bool] -> Bool
conjuncion   []   = True
conjuncion (x:xs) = x && conjuncion xs

disyuncion :: [Bool] -> Bool
disyuncion   []   = False
disyuncion (x:xs) = x || disyuncion xs 

aplanar :: [[a]] -> [a]
aplanar   []   = []
aplanar (x:xs) = x ++ aplanar xs     

pertenece :: Eq a => a -> [a] -> Bool
pertenece e   []   = False
pertenece e (x:xs) = e == x || pertenece e xs     

--Preguntar
apariciones :: Eq a => a -> [a] -> Int
apariciones e   []   = 0
apariciones e (x:xs) = unoSi (x == e) + apariciones e xs
--Subtarea    
unoSi :: Bool -> Int
unoSi True = 1
unoSi  _   = 0

losMenoresA :: Int -> [Int] -> [Int]
losMenoresA n   []   = []
losMenoresA n (x:xs) = if n > x 
                          then x : losMenoresA n xs
                          else losMenoresA n xs
                        
lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA n   []   = []
lasDeLongitudMayorA n (x:xs) = if longitud x > n 
                                  then x : lasDeLongitudMayorA n xs
                                  else lasDeLongitudMayorA n xs

agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal   []   y = y : []
agregarAlFinal (x:xs) y = x : agregarAlFinal xs y

agregar :: [a] -> [a] -> [a]
agregar   []     []   = []
agregar (x:xs)   l2   = x : agregar xs l2
agregar   l1   (y:ys) = y : agregar l1 ys

reversa :: [a] ->  [a]
reversa   []   = []
reversa (x:xs) = agregarAlFinal (reversa xs) x

zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos   []     []   = [] 
zipMaximos   []   (y:ys) = y : zipMaximos [] ys
zipMaximos (x:xs)   []   = x : zipMaximos xs []
zipMaximos (x:xs) (y:ys) = maxDelPar (x,y) : zipMaximos xs ys

maxDelPar :: (Int,Int) -> Int
maxDelPar (n, m) = if (n > m)
                       then n
                       else m

elMinimo :: Ord a => [a] -> a
elMinimo   []   = error "La lista está vacía"
elMinimo   [x]  = x
elMinimo (x:xs) = min x (elMinimo xs)

--2. Recursión sobre números

factorial :: Int -> Int --Precondicion: n debe ser un numero natural
factorial 0 = 1
factorial n = n * factorial (n-1)

cuentaRegresiva :: Int -> [Int] --Precondicion: n debe ser un numero natural
cuentaRegresiva 0 = []
cuentaRegresiva n = n : cuentaRegresiva (n-1)

repetir :: Int -> a -> [a]
repetir 0 e = [] 
repetir n e = e : repetir (n-1) e

losPrimeros :: Int -> [a] -> [a]
losPrimeros 0   _    = []
losPrimeros _   []   = []
losPrimeros n (x:xs) = x : losPrimeros (n-1) xs 

sinLosPrimeros :: Int -> [a] -> [a]
sinLosPrimeros 0   xs   = xs
sinLosPrimeros _   []   = [] 
sinLosPrimeros n (_:xs) = sinLosPrimeros (n-1) xs


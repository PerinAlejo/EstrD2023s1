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


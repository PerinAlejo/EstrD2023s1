--1. Tipos recursivos simples

--1.1. Celdas con bolitas

data Color = Azul | Rojo
        deriving Show
data Celda = Bolita Color Celda | CeldaVacia
        deriving Show

------------------------------------------------------------------------------------
nroBolitas :: Color -> Celda -> Int
nroBolitas _   CeldaVacia    = 0 
nroBolitas co (Bolita cc ce) = unoSi(esElMismoColor co cc) + nroBolitas co ce

esElMismoColor :: Color -> Color -> Bool
esElMismoColor Azul Azul = True
esElMismoColor Rojo Rojo = True
esElMismoColor  _    _   = False

unoSi :: Bool -> Int
unoSi True = 1
unoSi  _   = 0

------------------------------------------------------------------------------------
poner :: Color -> Celda -> Celda
poner co c = (Bolita co c) 

------------------------------------------------------------------------------------
sacar :: Color -> Celda -> Celda
sacar _    CeldaVacia   = CeldaVacia
sacar co (Bolita cc ce) = if esElMismoColor co cc
                            then ce
                            else (Bolita cc (sacar co ce))

------------------------------------------------------------------------------------
ponerN :: Int -> Color -> Celda -> Celda
ponerN 0 _  ce = ce 
ponerN n co ce = (Bolita co (ponerN (n-1) co ce))  

------------------------------------------------------------------------------------
--1.2. Camino hacia el tesoro

data Objeto = Cacharro | Tesoro
        deriving Show

data Camino = Fin | Cofre [Objeto] Camino | Nada Camino
        deriving Show

------------------------------------------------------------------------------------
hayTesoro :: Camino -> Bool
hayTesoro     Fin      = False
hayTesoro   (Nada c)   = hayTesoro c
hayTesoro (Cofre os c) = tieneTesoro os || hayTesoro c

tieneTesoro :: [Objeto] -> Bool
tieneTesoro   []   = False
tieneTesoro (o:os) = esTesoro o || tieneTesoro os

esTesoro :: Objeto  -> Bool
esTesoro Tesoro = True
esTesoro   _    = False

------------------------------------------------------------------------------------
pasosHastaTesoro :: Camino -> Int  --PREC: Debe haber un tesoro
pasosHastaTesoro     Fin      = error "No hay tesoro"
pasosHastaTesoro   (Nada c)   = 1 + pasosHastaTesoro c
pasosHastaTesoro (Cofre os c) = if tieneTesoro os 
                                then 0
                                else 1 + pasosHastaTesoro c

------------------------------------------------------------------------------------
hayTesoroEn :: Int -> Camino -> Bool
hayTesoroEn 0      c       = hayTesoroEnLugarActual c
hayTesoroEn _     Fin      = False
hayTesoroEn n   (Nada c)   = hayTesoroEn (n-1) c
hayTesoroEn n (Cofre os c) = hayTesoroEn (n-1) c
 
hayTesoroEnLugarActual :: Camino -> Bool
hayTesoroEnLugarActual (Cofre os _) = tieneTesoro os
hayTesoroEnLugarActual      _       = False

------------------------------------------------------------------------------------
alMenosNTesoros :: Int -> Camino -> Bool
alMenosNTesoros n c = cantidadDeTesoros c >= n

cantidadDeTesoros :: Camino -> Int
cantidadDeTesoros     Fin      = 0
cantidadDeTesoros   (Nada c)   = cantidadDeTesoros c
cantidadDeTesoros (Cofre os c) = unoSi(tieneTesoro os) + cantidadDeTesoros c

------------------------------------------------------------------------------------
cantTesorosEntre :: Int -> Int -> Camino -> Int
cantTesorosEntre n1 n2     Fin      = 0
cantTesorosEntre n1 n2   (Nada c)   = cantTesorosEntre n1 n2 c
cantTesorosEntre n1 n2 (Cofre os c) = cantTesorosEntre n1 n2 c






--2. Tipos arbóreos
--2.1. Árboles binarios

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
        deriving Show

------------------------------------------------------------------------------------
sumarT :: Tree Int -> Int
sumarT       EmptyT        = 0 
sumarT (NodeT n (a1) (a2)) = n + sumarT a1 + sumarT a2

------------------------------------------------------------------------------------
sizeT :: Tree a -> Int
sizeT        EmptyT       = 0
sizeT (NodeT n (a1) (a2)) = 1 + sizeT a1 + sizeT a2 

------------------------------------------------------------------------------------
mapDobleT :: Tree Int -> Tree Int
mapDobleT       EmptyT        = EmptyT
mapDobleT (NodeT n (a1) (a2)) = (NodeT (n*2) (mapDobleT a1) (mapDobleT a2))

------------------------------------------------------------------------------------
perteneceT :: Eq a => a -> Tree a -> Bool
perteneceT x       EmptyT        = False
perteneceT x (NodeT xa (a1) (a2)) = x == xa || perteneceT x a1 || perteneceT x a2

------------------------------------------------------------------------------------
aparicionesT :: Eq a => a -> Tree a -> Int
aparicionesT x       EmptyT         = 0
aparicionesT x (NodeT xa (a1) (a2)) = unoSi(x==xa) + (aparicionesT x a1) + (aparicionesT x a2)

------------------------------------------------------------------------------------
leaves :: Tree a -> [a]
leaves       EmptyT        = []
leaves (NodeT e (a1) (a2)) = e : leaves a1 ++ leaves a2  

------------------------------------------------------------------------------------
heightT :: Tree a -> Int
heightT       EmptyT        = 0
heightT (NodeT e (a1) (a2)) = 1 + max (heightT a1) (heightT a2)

------------------------------------------------------------------------------------
mirrorT :: Tree a -> Tree a
mirrorT       EmptyT        = EmptyT
mirrorT (NodeT e (a1) (a2)) = (NodeT e (mirrorT a2) (mirrorT a1))

------------------------------------------------------------------------------------
toList :: Tree a -> [a]
toList        EmptyT       = []
toList (NodeT e (a1) (a2)) = toList a1 ++ [e] ++ toList a2

------------------------------------------------------------------------------------
levelN :: Int -> Tree a -> [a]
levelN _        EmptyT       = []
levelN 0 (NodeT e (a1) (a2)) = e : []
levelN n (NodeT e (a1) (a2)) = levelN (n-1) a1 ++ levelN (n-1) a2

------------------------------------------------------------------------------------
listPerLevel :: Tree a -> [[a]]
listPerLevel       EmptyT       = []
listPerLevel (NodeT e (a1) (a2)) = [e] : juntarNiveles (listPerLevel a1) (listPerLevel a2)

juntarNiveles :: [[a]] -> [[a]] -> [[a]]
juntarNiveles    []       yss   = yss
juntarNiveles    xss      []    = xss
juntarNiveles (xs:xss) (ys:yss) = (xs ++ ys) : juntarNiveles xss yss

------------------------------------------------------------------------------------
ramaMasLarga :: Tree a -> [a]
ramaMasLarga       EmptyT       = []
ramaMasLarga (NodeT e (a1) (a2)) = e : laMasLarga (ramaMasLarga a1) (ramaMasLarga a2)

laMasLarga :: [a] -> [a] -> [a]
laMasLarga xs ys = if (length ys) > (length xs)             
                      then ys
                      else xs

------------------------------------------------------------------------------------
todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos       EmptyT        = []
todosLosCaminos (NodeT e (a1) (a2)) = [e] : consACada e (todosLosCaminos a1) ++ consACada e (todosLosCaminos a2)

consACada :: a -> [[a]] -> [[a]]
consACada x    []    = []
consACada x (xs:xss) = (x:xs) : consACada x xss


t1 = (NodeT 1 (NodeT 2(NodeT 3(EmptyT)(NodeT 4 (EmptyT)(EmptyT)))(EmptyT)) (NodeT 3(NodeT 4(EmptyT)(EmptyT)) EmptyT))


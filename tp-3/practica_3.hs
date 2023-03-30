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
-- hayTesoroEn :: Int -> Camino -> Bool

------------------------------------------------------------------------------------
alMenosNTesoros :: Int -> Camino -> Bool
alMenosNTesoros n c = cantidadDeTesoros c >= n

cantidadDeTesoros :: Camino -> Int
cantidadDeTesoros     Fin      = 0
cantidadDeTesoros   (Nada c)   = cantidadDeTesoros c
cantidadDeTesoros (Cofre os c) = unoSi(tieneTesoro os) + cantidadDeTesoros c

-- alMenosNTesoros 2 (Nada (Cofre [Tesoro] (Cofre [Tesoro] Fin)))

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
-- levelN :: Int -> Tree a -> [a]
-- levelN n        EmptyT       = 
-- levelN n (NodeT e (a1) (a2)) = 




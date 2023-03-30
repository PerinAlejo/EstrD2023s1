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
hayTesoroEn _     Fin      = False
hayTesoroEn n   (Nada c)   = pasosHastaTesoro c == n
hayTesoroEn n (Cofre os c) = pasosHastaTesoro c == n

                                
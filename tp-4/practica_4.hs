--1. Pizzas

data Ingrediente = Salsa | Queso | Jamon | Aceitunas Int
          deriving Show
data Pizza = Prepizza | Capa Ingrediente Pizza
          deriving Show

ing1 = [Aceitunas 5, Jamon, Queso, Salsa]
ing2 = [Jamon, Queso, Salsa]
ing3 = [Queso, Salsa]

------------------------------------------------------------------------------------
cantidadDeCapas :: Pizza -> Int
cantidadDeCapas  Prepizza  = 0
cantidadDeCapas (Capa i p) = 1 + cantidadDeCapas p

------------------------------------------------------------------------------------
armarPizza :: [Ingrediente] -> Pizza
armarPizza   []   = Prepizza
armarPizza (i:is) = (Capa i (armarPizza is))

------------------------------------------------------------------------------------
sacarJamon :: Pizza -> Pizza
sacarJamon  Prepizza  = Prepizza
sacarJamon (Capa i p) = if esJamon i
                        then sacarJamon p
                        else (Capa i (sacarJamon p))

esJamon :: Ingrediente -> Bool
esJamon Jamon = True
esJamon   _   = False

------------------------------------------------------------------------------------
tieneSoloSalsaYQueso :: Pizza -> Bool
tieneSoloSalsaYQueso  Prepizza  = True
tieneSoloSalsaYQueso (Capa i p) = esSalsaOQueso i && tieneSoloSalsaYQueso p

esSalsaOQueso :: Ingrediente -> Bool
esSalsaOQueso Salsa = True
esSalsaOQueso Queso = True
esSalsaOQueso   _   = False

------------------------------------------------------------------------------------
duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas  Prepizza  = Prepizza
duplicarAceitunas (Capa i p) = (Capa (duplicarSiEsAceituna i) (duplicarAceitunas p))

duplicarSiEsAceituna :: Ingrediente -> Ingrediente
duplicarSiEsAceituna (Aceitunas n) = (Aceitunas (n*2))
duplicarSiEsAceituna       i       = i

------------------------------------------------------------------------------------
cantCapasPorPizza :: [Pizza] -> [(Int, Pizza)]
cantCapasPorPizza   []   = []
cantCapasPorPizza (p:ps) = (cantidadDeCapas p , p) : cantCapasPorPizza ps

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
--2. Mapa de tesoros (con bifurcaciones)

data Dir = Izq | Der
          deriving Show
data Objeto = Tesoro | Chatarra
          deriving Show
data Cofre = Cofre [Objeto]
          deriving Show
data Mapa = Fin Cofre | Bifurcacion Cofre Mapa Mapa
          deriving Show
ct = (Cofre [Tesoro])
cc = (Cofre [Chatarra])
m1 = (Bifurcacion cc 
          (Bifurcacion ct 
               (Bifurcacion cc 
                    (Fin ct)
                    (Bifurcacion cc 
                         (Bifurcacion ct 
                              (Fin cc)
                              (Fin cc))
                         (Fin ct)))
               (Fin cc))
          (Fin ct))

------------------------------------------------------------------------------------
hayTesoro :: Mapa -> Bool
hayTesoro        (Fin c)        = hayTesoroEnCofre c
hayTesoro (Bifurcacion c m1 m2) = hayTesoroEnCofre c || hayTesoro m1 || hayTesoro m2

hayTesoroEnCofre :: Cofre -> Bool
hayTesoroEnCofre (Cofre os) = tieneTesoro os 

tieneTesoro :: [Objeto] -> Bool
tieneTesoro   []   = False
tieneTesoro (o:os) = esTesoro o || tieneTesoro os

esTesoro :: Objeto  -> Bool
esTesoro Tesoro = True
esTesoro   _    = False

------------------------------------------------------------------------------------
hayTesoroEn :: [Dir] -> Mapa -> Bool
hayTesoroEn   []             m           = hayCofrePosicionActual m 
hayTesoroEn (d:ds)        (Fin c)        = error "No hay camino para recorrer"
hayTesoroEn (d:ds) (Bifurcacion c m1 m2) = if esIzq d 
                                           then hayTesoroEn ds m1
                                           else hayTesoroEn ds m2

hayCofrePosicionActual :: Mapa -> Bool
hayCofrePosicionActual       (Fin c)       = hayTesoroEnCofre c
hayCofrePosicionActual (Bifurcacion c _ _) = hayTesoroEnCofre c

esIzq :: Dir -> Bool
esIzq Izq = True
esIzq  _  = False

------------------------------------------------------------------------------------
caminoAlTesoro :: Mapa -> [Dir] --PREC: existe un tesoro y es único
caminoAlTesoro         (Fin c)       = []
caminoAlTesoro (Bifurcacion c m1 m2) = if hayTesoro m1
                                       then Izq : caminoAlTesoro m1
                                       else if hayTesoro m2 
                                            then Der : caminoAlTesoro m2
                                            else []

------------------------------------------------------------------------------------
caminoDeLaRamaMasLarga :: Mapa -> [Dir]
caminoDeLaRamaMasLarga         (Fin c)       = []
caminoDeLaRamaMasLarga (Bifurcacion c m1 m2) = if longitudDeCamino m1 > longitudDeCamino m2
                                               then Izq : caminoDeLaRamaMasLarga m1 
                                               else Der : caminoDeLaRamaMasLarga m2

longitudDeCamino :: Mapa -> Int
longitudDeCamino         (Fin c)       = 0
longitudDeCamino (Bifurcacion c m1 m2) = 1 + max (longitudDeCamino m1) (longitudDeCamino m2)

------------------------------------------------------------------------------------
tesorosPorNivel :: Mapa -> [[Objeto]]
tesorosPorNivel         (Fin c)       = [tesoros c]
tesorosPorNivel (Bifurcacion c m1 m2) = tesoros c : juntarNiveles (tesorosPorNivel m1) (tesorosPorNivel m2 )

juntarNiveles :: [[a]] -> [[a]] -> [[a]]
juntarNiveles    []       yss   = yss
juntarNiveles    xss      []    = xss
juntarNiveles (xs:xss) (ys:yss) = (xs ++ ys) : juntarNiveles xss yss

tesoros :: Cofre -> [Objeto]
tesoros (Cofre os) = tesorosEnObjetos os

tesorosEnObjetos :: [Objeto] -> [Objeto]
tesorosEnObjetos   []   = []
tesorosEnObjetos (o:os) = singularSi o (esTesoro o) ++ tesorosEnObjetos os

singularSi :: a -> Bool -> [a]
singularSi a True = [a]
singularSi a  _   = []

------------------------------------------------------------------------------------
todosLosCaminos :: Mapa -> [[Dir]]
todosLosCaminos         (Fin c)       = [[]]
todosLosCaminos (Bifurcacion c m1 m2) = consACada Izq (todosLosCaminos m1 ) ++ consACada Der (todosLosCaminos m2)

consACada :: a -> [[a]] -> [[a]]
consACada x    []    = []
consACada x (xs:xss) = (x:xs) : consACada x xss
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
--3. Nave Espacial

data Componente = LanzaTorpedos | Motor Int | Almacen [Barril]
          deriving Show
data Barril = Comida | Oxigeno | Torpedo | Combustible
          deriving Show
data Sector = S SectorId [Componente] [Tripulante]
          deriving Show
type SectorId = String
type Tripulante = String
data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
          deriving Show
data Nave = N (Tree Sector)
          deriving Show

------------------------------------------------------------------------------------
sectores :: Nave -> [SectorId] 


------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
--4. Manada de lobos

type Presa = String -- nombre de presa
type Territorio = String -- nombre de territorio
type Nombre = String -- nombre de lobo
data Lobo   = Cazador Nombre [Presa] Lobo Lobo Lobo
            | Explorador Nombre [Territorio] Lobo Lobo
            | Cria Nombre
data Manada = M Lobo

cria = (Cria "Jose")
l1 = (Cazador "Juan" ["Conejo", "Conejo"] cria cria (Cazador "Alfa" ["Conejo", "Conejo", "Conejo"] cria cria cria))

------------------------------------------------------------------------------------
elAlfa :: Manada -> (Nombre, Int)
elAlfa (M l) = (nombreLobo (elAlfaDeLobo l), cantPresas (elAlfaDeLobo l))

nombreLobo :: Lobo -> String
nombreLobo     (Cria n)         = n
nombreLobo (Explorador n _ _ _) = n
nombreLobo (Cazador n _ _ _ _)  = n

cantPresas:: Lobo -> Int
cantPresas (Cazador _ ps _ _ _) = length ps
cantPresas _                    = 0

elAlfaDeLobo :: Lobo -> Lobo
elAlfaDeLobo        (Cria n)         = (Cria n)
elAlfaDeLobo (Explorador n ts l1 l2) = elQueMasCazo [(Explorador n ts l1 l2), elAlfaDeLobo l1, elAlfaDeLobo l2]
elAlfaDeLobo (Cazador n ps l1 l2 l3) = elQueMasCazo [(Cazador n ps l1 l2 l3), elAlfaDeLobo l1, elAlfaDeLobo l2, elAlfaDeLobo l3]

elQueMasCazo :: [Lobo] -> Lobo
elQueMasCazo  [ ]   = error "no hay Lobo"
elQueMasCazo  [l]   = l
elQueMasCazo (l:ls) = if elPrimeroEsMasAlfa l (elQueMasCazo ls)
                      then l
                      else elQueMasCazo ls

elPrimeroEsMasAlfa :: Lobo -> Lobo -> Bool
elPrimeroEsMasAlfa (Cazador _ ps1 _ _ _) (Cazador _ ps2 _ _ _) = length ps1 > length ps2
elPrimeroEsMasAlfa           l1                     _          = esCazador l1

esCazador :: Lobo -> Bool
esCazador (Cazador _ _ _ _ _) = True
esCazador          _          = False

------------------------------------------------------------------------------------
-- let nombre = expresion in codigo
-- superioresDelCazador :: Nombre -> Manada -> [Nombre]
-- superioresDelCazador n (M l) = nombresDe (soloCazadores(todosLosSuperiores n l))

todosLosSuperiores :: Nombre -> Lobo -> [Lobo]
todosLosSuperiores n        (Cria nl)         = [(Cria nl)]
todosLosSuperiores n (Explorador nl ts l1 l2) = if nombreLobo l1 == n || nombreLobo l2 == n
                                                then [Explorador nl ts l1 l2]
                                                else if not (null(todosLosSuperiores n l1 ++ todosLosSuperiores n l2))
                                                     then (Explorador nl ts l1 l2) : (todosLosSuperiores n l1 ++ todosLosSuperiores n l2)
                                                     else []
                                                
todosLosSuperiores n (Cazador nl ps l1 l2 l3) = if nombreLobo l1 == n || nombreLobo l2 == n || nombreLobo l3 == n
                                                then [(Cazador nl ps l1 l2 l3)]
                                                else if not (null(todosLosSuperiores n l1++todosLosSuperiores n l2++todosLosSuperiores n l3))
                                                     then (Cazador nl ps l1 l2 l3) : (todosLosSuperiores n l1 ++ todosLosSuperiores n l2 ++todosLosSuperiores n l3)
                                                     else []    

                                                
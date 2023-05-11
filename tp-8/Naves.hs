module Nave 
    (Nave)
where 

import Sector -- Contiene componentes y tripulantes asignados.
import Tripulante -- Tiene un nombre, un rango y sectores asignados
import Map
import Set
import MaxHeap

data Componente = LanzaTorpedos | Motor Int | Almacen [Barril]
data Barril = Comida | Oxigeno | Torpedo | Combustible

type SectorId = String --Sector unico
type Nombre   = String --Nombre unico 
type Rango    = String

-- Un sector está vacío cuando no tiene tripulantes, y la nave está vacía si no tiene ningún tripulante.
-- Puede haber tripulantes sin sectores asignados

data Nave = N (Map SectorId Sector) (Map Nombre Tripulante) (MaxHeap Tripulante)
{-
INV.REP: Para N ms mt ht:
    -en mt el Tripulante debe tener el mismo nombre al que esta asignado.
    -en ms el sector debe tener un SectorId igual al que tiene asignado
    -no hay repetidos en ht
    -todo tripulante en mt pertenece a ht y viceversa.
    -sea T un tripulante y S un sector:
        nombre T pertenece a tripulante S si y solo si soctorId S pertenece a sectores T
-}

--Propósito: Construye una nave con sectores vacíos, en base a una lista de identificadores de sectores.
--Eficiencia: O(S). El rango es O(S log S)
construir :: [SectorId] -> Nave 
construir sIds = N (construirMap sIds) emptyM emptyH

--Eficiencia: El peor caso es donde la lista tiene componentes donde seria O(S log S) debido a que se hace una operacion logS 
--por cada elemento de la lista
construirMap :: [SectorId] -> Map SectorId Sector
construirMap   []   = emptyM --O(1)
construirMap (s:ss) = assocM s (crearS s) (construirMap ss) --O(S log S) 
--                    O(log S)    O(1)            

-----------------------------------------------------------------------------
--Propósito: Incorpora un tripulante a la nave, sin asignarle un sector.
--Eficiencia: O(log T)
ingresarT :: Nombre -> Rango -> Nave -> Nave
ingresarT n r (N ms mt ht) = N ms (agregarTripulanteM n r mt) (agregarTripulanteH n r ht)

--Eficiencia: El costo de la operacion seria O(log T)
agregarTripulanteM :: Nombre -> Rango -> Map Nombre Tripulante -> Map Nombre Tripulante
agregarTripulanteM n r ms = assocM n (crearT n r) ms 
                 --          log T      1      

--Eficiencia: El costo de la operacion seria O(log T)                          
agregarTripulanteH :: Nombre -> Rango -> MaxHeap Tripulante -> MaxHeap Tripulante
agregarTripulanteH n r mh = insertH (crearT n r) mh
                 --          log T      1  

-----------------------------------------------------------------------------
--Propósito: Devuelve los sectores asignados a un tripulante.
--Precondición: Existe un tripulante con dicho nombre.
--Eficiencia: O(log M)
sectoresAsignados :: Nombre -> Nave -> Set SectorId
sectoresAsignados n (N _ _ ht) =  

-----------------------------------------------------------------------------
--Propósito: Asigna un sector a un tripulante.
--Nota: No importa si el tripulante ya tiene asignado dicho sector.
--Precondición: El tripulante y el sector existen.
--Eficiencia: O(log S + log T + T log T)
asignarASector :: Nombre -> SectorId -> Nave -> Nave
asignarASector n id (N ms mt ht) = let newT = asignarA id (fromJust(lookupM n mt))     --log T 
                                       newS = agregarT n  (fromJust(lookupM id ms)) in --log S
                                    N (assocM id sewS ms) (assocM n newT mt) (reemplazarEnHeap n newT ht)
--                                      log S               log T                  T log T

reemplazarEnHeap :: Nombre -> Tripulante -> MaxHeap Tripulante -> MaxHeap Tripulante
reemplazarEnHeap n t ht = insertH t (borrarH n ht)
--                        O(log T)     O(T log T)

--Eficiencia: Como estoy haciendo una operacion logaritmica T cantidad de veces, la Operacion quedaria como 
--O(T * log T + logT) , por lo que quedaria como O(T log T)
borrarH :: Nombre -> MaxHeap Tripulante -> MaxHeap Tripulante
borrarH n ht = let t = maxH ht in 
                 if nombre(t) == n 
                    then deleteMaxH ht
                    else insertH t (borrarH n (deleteMaxH ht)) -- 
--                        O(log T)     T          O(log T)
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

--Eficiencia: El peor caso es donde la lista tiene componentes donde seria O(S log S) debido a que se hace una operacion logS por cada elemento de la lista
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
--Eficiencia: O(log M). Si se debe encontrar los SecotorId sobre la estuctura del ht la funcion quedaria como O(S log M). Para que 
--sea una funcion logaritmica se deberia buscar sobre la estructura del ms
sectoresAsignados :: Nombre -> Nave -> Set SectorId
sectoresAsignados n (N _ _ ht) = sectoresH n ht

--Eficiencia: O(log )
sectoresH :: Nombre -> MaxHeap Tripulante -> Set SectorId
sectoresH n ht = let t = maxH ht in --1
                    if nombre t == n 
                        then sectoresT t --1
                        else sectoresH (deleteMaxH ht)
--                                         log M

-----------------------------------------------------------------------------
--Propósito: Dado un sector, devuelve los tripulantes y los componentes asignados a ese sector.
--Precondición: Existe un sector con dicho id.
--Eficiencia: O(log S)
datosDeSector :: SectorId -> Nave -> (Set Nombre, [Componente]) 
datosDeSector id (N hs _ _) = case lookupM id hs of  --log S
                                Nothing -> error "No existe el tripulante"
                                Just s -> nombresYComponentes s
                                    --            1  

--Eficiencia: O(1)
nombresYComponentes :: Sector -> (Set Nombre, [Componente])
nombresYComponentes s = (tripulantesS s, componentesS s)
                    --        1               1     

-----------------------------------------------------------------------------
--Propósito: Devuelve la lista de tripulantes ordenada por rango, de mayor a menor.
--Eficiencia: O(log T). No coincide con la eficiencia conseguida ya que esta seria O(T log T)
tripulantesN :: Nave -> [Tripulante]
tripulantesN (N _ _ ht) = tripulantesH ht
--                          O(T log T)

--Eficiecia: O(T log T)
tripulantesH :: MaxHeap Tripulante -> [Tripulante]
tripulantesH ht = if isEmptyH ht
                    then []
                    else maxH ht : tripulantesH (deleteMaxH ht)
--                        O(1)         T           O(log T)

-----------------------------------------------------------------------------
--Propósito: Asigna una lista de componentes a un sector de la nave.
--Eficiencia: O(C + log S), siendo C la cantidad de componentes dados.
agregarASector :: [Componente] -> SectorId -> Nave -> Nave
agregarASector cs id (N ms mt ht) = case lookupM id ms of --log S
                                    Nothing -> N ms mt ht
                                    Just s  -> N (agregarC cs s) mt ht
--                                                   C                          

--Eficiencia: O(C)
agregarCS :: [Componente] -> Sector -> Sector
agregarCS   []   s = s
agregarCS (c:cs) s = agregarC c (agregarCS cs s)
--                       1           C
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
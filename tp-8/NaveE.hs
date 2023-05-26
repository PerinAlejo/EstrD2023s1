module NaveE
    (Nave,)
where

import Rango
import Tripulante
import Sector

data Nave = MkN (Map Sector (Set Tripulante)) (Heap Tripulante) (Sector, Int)
{-
INV.REP: Para MkN ms h (s,n):
    - en ms cada tripulante debe pertenecer a un unico sector.
    - no debe haber tripulantes repetidos en h.
    - h esta ordenado de mayor rango del tripulante a menor rango.
    - la cantidad de tripulantes en h y ms debe ser la misma.
    - en (s,n) s debe ser el sector como mas tripulantes de la nave y n debe representar esa cantidad.
-}

--Propósito: Crea una nave con todos esos sectores sin tripulantes.
--Precondición: la lista de sectores no está vacía
--Costo: O(S log S) siendo S la cantidad de sectores de la lista.
naveVacia :: [Sector] -> Nave
naveVacia ls = MkN (mapSector ls) emptyH (head ls,0)
--                   O(S log S)    O(1)     O(1)

--Costo: O(S log S)
mapSector :: [Sector] -> Map Sector (Set Tripulante)
mapSector   []   = emptyM
mapSector (s:ss) = assocM s emptyS (mapSector ss)

-----------------------------------------------------------------------------
--Propósito: Obtiene los tripulantes de un sector.
--Costo: O(log S) siendo S la cantidad de sectores.
tripulantesDe :: Sector -> Nave -> Set Tripulante
tripulantesDe s (MkN ms _ _) = case (lookupM s ms) of
                                    Nothing -> emptyS
                                    Just st -> st

-----------------------------------------------------------------------------  
--Propósito: Denota los sectores de la nave
--Costo: O(S) siendo S la cantidad de sectores.
sectores :: Nave -> [Sector]
sectores (MkN ms _ _) = domM ms

-----------------------------------------------------------------------------
--Propósito: Denota el tripulante con mayor rango.
--Precondición: la nave no está vacía.
--Costo: O(1).
conMayorRango :: Nave -> Tripulante
conMayorRango (MkN _ h _) = findMin n

-----------------------------------------------------------------------------
--Propósito: Denota el sector de la nave con más tripulantes.
--Costo: O(1).
conMasTripulantes :: Nave -> Sector
conMasTripulantes (MkN _ _ (s,n)) = s

-----------------------------------------------------------------------------
--Propósito: Denota el conjunto de tripulantes con dicho rango.
--Costo: O(P log P) siendo P la cantidad de tripulantes
conRango :: Rango -> Nave -> Set Tripulante
conRango r (MkN _ h _) = conRangoH r h

conRangoH :: Rango -> Heap -> Set Tripulante
conRangoH r h = let t = findMin hin 
                if r == rango t 
                then addS t (conRangoH r (deleteMin h))
                else conRangoH r (deleteMin h)

-----------------------------------------------------------------------------
--Propósito: Devuelve el sector en el que se encuentra un tripulante.
--Precondición: el tripulante pertenece a la nave.
--Costo: O(S log S log P) siendo S la cantidad de sectores y P la cantidad de tripulantes.
sectorDe :: Tripulante -> Nave -> Sector
sectorDe t (MkN ms _ _) = sectorDeT (domM ms) t ms

--Prec: el tripulante pertenece a alguno de los sectores
sectorDeT :: [Sector] -> Tripulante -> Map Sector (Set Tripulante)
sectorDeT (s:ss) t ms = let st = fromJust(lookupM s ms) in 
                        if belongs t st -- O(log P)
                        then s
                        else sectorDeT ss t ms 

-----------------------------------------------------------------------------
--Propósito: Agrega un tripulante a ese sector de la nave.
--Precondición: El sector está en la nave y el tripulante no.
agregarTripulante :: Tripulante -> Sector -> Nave -> Nave
agregarTripulante t s (MkN ms h sn) = Mkn (assocM s t ms) (insertH t h) (recalcularSConMasT s ms sn)

recalcularSConMasT :: Sector -> Map Sector (Set Tripulante) -> (Sector, Int) -> (Sector, Int)
recalcularSConMasT s ms (sn,n) = let tamañoNuevoS = sizeS(fromJust(lookupM s ms)) in
                                 if n < tamañoNuevoS
                                    then (s,tamañoNuevoS)
                                    else (sn,n)
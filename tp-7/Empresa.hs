module Empresa 
    (Empresa, )
where

import Empleado
import MapV3
import SetV1

type SectorId = Int
type CUIL = Int

data Empresa = ConsE (Map SectorId (Set Empleado))
                     (Map CUIL Empleado)

{-
INV. REP:
    - Los SectorId de los empleados deben pertenecer a la empresa
    - Cada empleado del Set debe poseer el sectorId al que esta asignado.
    - Cada empleado debe poseer el CUIL al que esta asignado.
    - Cada empleado del conjunto debe perteneces al map Culi Empleado.
-}

-----------------------------------------------------------------------------------
--Propósito: construye una empresa vacía
--O(1)
consEmpresa :: Empresa
consEmpresa = ConsE emptyM emptyM  

-----------------------------------------------------------------------------------
--Propósito: devuelve el empleado con dicho CUIL.
--Costo: O(log E)
buscarPorCUIL :: CUIL -> Empresa -> Empleado
buscarPorCUIL c (ConsE _ mc) = case (lookupM c mc) of 
                                Nothing -> error "El Empleado no Existe"
                                Just e  -> e

-----------------------------------------------------------------------------------
--Propósito: indica los empleados que trabajan en un sector dado.
--Costo: O(logS + E)  si set to list es lineal y si es constante queda O(logS)
empleadosDelSector ::SectorId -> Empresa -> [Empleado]
empleadosDelSector id (ConsE ms _) = case (lookupM id ms) of   --logS
                                      Nothing -> error "El Sector no existe"
                                      Just se -> setToList se  --O(1)

-----------------------------------------------------------------------------------
--Propósito: indica todos los CUIL de empleados de la empresa.
--Costo: O(E) ya que hay la misma cantidad de empleados que de cuils y keys es O(K)
todosLosCUIL :: Empresa -> [CUIL]
todosLosCUIL (ConsE _ mc) = keys mc

-----------------------------------------------------------------------------------
--Propósito: indica todos los sectores de la empresa.
--Costo: O(S) ya que keys es O(K) y en este caso K=S
todosLosSectores :: Empresa -> [SectorId]
todosLosSectores (ConsE ms _)= keys ms

-----------------------------------------------------------------------------------
--Propósito: agrega un sector a la empresa, inicialmente sin empleados.
--Costo: O(logS) ya que (lookupM id ms) y (assocM id emptyS ms) son O(logS) y logS + logS = logS
agregarSector :: SectorId -> Empresa -> Empresa
agregarSector id (ConsE ms mc) = case (lookupM id ms) of 
                                    Nothing -> ConsE (assocM id emptyS ms) mc
                                    Just s  -> error  "El sector ya existe"
 
-----------------------------------------------------------------------------------
--Propósito: agrega un empleado a la empresa, en el que trabajará en dichos sectores y tendrá el CUIL dado.
--Costo: 
agregarEmpleado :: [SectorId] -> CUIL -> Empresa -> Empresa
agregarEmpleado ls c (ConsE ms mc) = case (lookupM c mc) of --O(log E)
                                        Just s  -> error  "El Empleado ya existe"
                                        Nothing -> let newE = nuevoEmpleado c ls in 
                                            (ConsE (agregarEmpleadoASec ms newE ls) (assocM c newE mc))
                                            --          O(N log S)                        O(log E)

--Costo: si N son los elementos de la lista el costo seria (N log S) ya que por cada elemento de la lista hace una operacion logaritmica
nuevoEmpleado :: CUIL -> [SectorId] -> Empleado
nuevoEmpleado c   []   = consEmpleado c  --O(1)
nuevoEmpleado c (s:ss) = incorporarSector s (nuevoEmpleado c ss)                                  

--Costo: suponiendo que addS es O(1) el costo seria  log S + log S *  N, donde N son los elementos de la lista, por lo que el costo seria 
--O(N log S)
agregarEmpleadoASec :: Map SectorId (Set Empleado) -> Empleado -> [SectorId] -> Map SectorId (Set Empleado)
agregarEmpleadoASec ms e   []   = ms
agregarEmpleadoASec ms e (s:ss) = case (lookupM s ms) of --O(log S)
                                    Nothing -> assocM s (addS e emptyS) (agregarEmpleadoASec ms e ss) 
                                    Just se -> assocM s (addS e se) (agregarEmpleadoASec ms e ss)     
                                            -- O(log S)    O(1)

-----------------------------------------------------------------------------------
--Propósito: agrega un sector al empleado con dicho CUIL.
--Costo: El costo seria O((n * log S + S) + log S + log S)  
agregarASector :: SectorId -> CUIL -> Empresa -> Empresa
agregarASector id c (ConsE ms mc) = case (lookupM c mc) of -- log S 
                                        Nothing -> error "No existe el empleado"
                                        Just e  -> let newE = incorporarSector id e  -- log S
                                                       secE = sectores newE in
                                            (ConsE (actualizarEmpleado newE secE ms) (assocM c newE mc))
                                                        -- O(n * log S + S)                  log S    

--Proposito: Retorna un map id con el empleado actualizado en los sectores que ya estaba y con el empleado agregado al nuevo sector.
--Costo: el costo seria O(n * log S + S) donde n son los elementos de la lista 
actualizarEmpleado :: Empleado -> [SectorId] -> Map SectorId (Set Empleado) ->Map SectorId (Set Empleado)
actualizarEmpleado e   []   ms = ms --O(1)
actualizarEmpleado e (s:ss) ms = 
                assocM s (actualizarSet e fromJust(lookupM s ms)) (actualizarEmpleado e ss ms)
                --log S         S                       log S               

--Costo: Suponiendo que belongs addS y removeS son lineales el costo de la operacion seria lineal + lineal + lineal
--por lo que queda O(S) donde S son los SecotoresId del empleado
actualizarSet :: Empleado -> Set Empleado -> Set Empleado
actualizarSet e s = if belongs e s  -- 
                    then addS e (removeS e s) -- 
                    else addS e s 

-----------------------------------------------------------------------------------
--Propósito: elimina al empleado que posee dicho CUIL.
borrarEmpleado :: CUIL -> Empresa -> Empresa
borrarEmpleado c (ConsE ms mc) = case (lookupM c mc) of
                                    Nothing -> error "El empleado no existe" 
                                    Just e  -> ConsE (borrarEmpleado (sectores e) e ms) (deleteM c mc)

--Borrar el empleado de todos los sectores a los que pertenezca
borrarEmpleado :: [SectoresId] -> Empleado -> Map SectorId (Set Empleado) -> Map SectorId (Set Empleado)


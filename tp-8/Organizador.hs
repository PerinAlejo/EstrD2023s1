module Organizador
    (Organizador,)
where

import Checksum  -- códigos identificadores de programas
import Persona   -- programadores
import Map
import Set
 

data Organizador = MkO (Map Checksum (Set Persona)) (Map Persona (Set Checksum))
{-
INV. REP.: para MkO mc mp 
    - En cado de que una persona tenga un checksum este debe coincidir con el checksum al que esta asignado
    - en mp el checksum debe estar asignado a la persona que lo contenga
    - Cada persona asignada a mc debe pertenecer a mp
    - Cada checksum asignado a mp debe pertenecer a mc
-}


--Propósito: Un organizador vacío.
--Eficiencia: O(1) se realizan dos operaciones O(1)
nuevo :: Organizador
nuevo = Mko emptyM emptyM

-----------------------------------------------------------------------------
--Propósito: Agrega al organizador un programa con el Checksum indicado; el conjunto es el conjunto de personas autores de dicho programa.
--Precondición: el identificador del programa que se agrega no fue usado previamente en el organizador, y el Set de personas no está vacío.
--Eficiencia: no hay ninguna garantía de eficiencia
agregarPrograma :: Organizador -> Checksum -> Set Persona -> Organizador
agregarPrograma (MkO mc mp) c sp = MkO (assocM c sp mc) (agregarChecksumAPersona c (set2list sp) mp)

--Prec: la lista no es vacia
agregarChecksumAPersona :: Checksum -> [Persona] -> Map Persona (Set Checksum) -> Map Persona (Set Checksum)
agregarChecksumAPersona _   []   mp = mp
agregarChecksumAPersona c (p:ps) mp = assocM p (checksumAgregado c p mp) (agregarChecksumAPersona c ps mp)  

--Prec: La persona debe existir en el map
checksumAgregado :: Checksum -> Persona -> Map Persona (Set Checksum) -> Set Checksum
checksumAgregado c p mp = case (lookupM p mp) of
                            Nothing -> error "La perosna no existe"
                            Just sc -> addS c sc

-----------------------------------------------------------------------------
--Propósito: denota una lista con todos y cada uno de los códigos identificadores de programas del organizador.
--Eficiencia: O(C) en peor caso, donde C es la cantidad de códigos en el organizador.
todosLosProgramas :: Organizador -> [Checksum]
todosLosProgramas (MkO mc _) = domM mc

-----------------------------------------------------------------------------
--Propósito: denota el conjunto de autores que aparecen en un programa determinado.
--Precondición: el Checksum debe corresponder a un programa del organizador.
--Eficiencia: O(log C) en peor caso, donde C es la cantidad total de programas del organizador.
autoresDe :: Organizador -> Checksum -> Set Persona
autoresDe (MkO mc _) c = lookupM c mc 

-----------------------------------------------------------------------------
--Propósito: denota el conjunto de programas en los que participó una determinada persona.
--Precondición: la persona debe existir en el organizador.
--Eficiencia: O(log P) en peor caso, donde P es la cantidad total de personas del organizador.
programasDe :: Organizador -> Persona -> Set Checksum
programasDe (MkO _ mp) p = lookupM p mp

-----------------------------------------------------------------------------
--Propósito: dado un organizador y dos personas, denota verdadero si ambas son autores de algún software en común.
--Precondición: las personas deben ser distintas y deben existir.
--Eficiencia: O(log P + C log C) en peor caso, donde P es la cantidad de personas distintas que aparecen en todos los programas del organizador, y C la cantidad total de programas.
programaronJuntas :: Organizador -> Persona -> Persona -> Bool
programaronJuntas (MkO _ mp) p1 p2 = let cP1 = lookupM p1 mp 
                                         cP2 = lookupM p2 mp in 
                                     programaronJuntasS cP1 cP2

programaronJuntasS :: Set Checksum -> Set Checksum -> Bool
programaronJuntasS sc1 sc2 = not (null (intersection sc1 sc2))

-----------------------------------------------------------------------------
--Propósito: dado un organizador y una persona, denota la cantidad de programas distintos en los que aparece.
--Eficiencia: O(log P) en peor caso, donde P es la cantidad de personas del organizador.
nroProgramasDePersona :: Organizador -> Persona -> Int
nroProgramasDePersona (MkO _ mp) p = case (lookupM p mp) of 
                                        Nothing -> 0
                                        Just sc -> sizeS sc
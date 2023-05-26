-- ==============================================================
-- =================== Nave Enterprise ==========================
-- ==============================================================

--Propósito: Denota los tripulantes de la nave
tripulantes :: Nave -> Set Tripulante
tripulantes n = tripulantesS n (sectores n)

tripulantesS :: Nave -> [Sector] -> Set Tripulante
tripulantesS n   []   =  emptyS
tripulantesS n (s:ss) =  union (tripulantesDe s n) (tripulantesS n ss)
--
-----------------------------------------------------------------------------
--Propósito: Elimina al tripulante de la nave.
bajaDeTripulante :: Tripulante -> Nave -> Nave

-- ==============================================================
-- =================== Nave =====================================
-- ==============================================================
--Propósito: Devuelve todos los sectores no vacíos (con tripulantes asignados).
sectores :: Nave -> Set SectorId
sectores n = sectoresN n (tripulantesN n)

sectoresN :: Nave -> [Tripulante] -> Set SectorId
sectoresN n   []   = emptyS  
sectoresN n (t:ts) = unionS (sectoresAsignados t) (sectoresN n ts)
--                 O(S log S)      O(log S)            


sinSectoresAsignados :: Nave ->[Tripulante]

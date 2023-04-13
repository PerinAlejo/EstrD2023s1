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
todosLosSuperiores n        (Cria n)          = 
todosLosSuperiores n (Explorador nl ts l1 l2) = if nombreLobo l1 == n || nombreLobo l2 == n
                                                then [Explorador nl ts l1 l2]
                                                else if not null(todosLosSuperiores n l1 ++ todosLosSuperiores n l2)
                                                     then (Explorador nl ts l1 l2) : (todosLosSuperiores n l1 ++ todosLosSuperiores n l2)
                                                
todosLosSuperiores n (Cazador nl ps l1 l2 l3) = if nombreLobo l1 == n || nombreLobo l2 == n || nombreLobo l3 == n
                                                then [Explorador nl ts l1 l2]
                                                else if not null(todosLosSuperiores n l1 ++ todosLosSuperiores n l2 ++todosLosSuperiores n l3)
                                                     then (Explorador nl ts l1 l2) : (todosLosSuperiores n l1 ++ todosLosSuperiores n l2 ++todosLosSuperiores n l3)    

                                                
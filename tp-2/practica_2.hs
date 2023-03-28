--1. Recursión sobre listas

sumatoria :: [Int] -> Int
--sumatoria   []   = 0
sumatoria (x:xs) = x + sumatoria xs

longitud :: [a] -> Int
longitud   []   = 0
longitud (_:xs) = 1 + longitud xs

sucesores :: [Int] -> [Int]
sucesores   []   = []
sucesores (x:xs) = x + 1 : sucesores xs    

--Preguntar
conjuncion :: [Bool] -> Bool
conjuncion   []   = True
conjuncion (x:xs) = x && conjuncion xs

disyuncion :: [Bool] -> Bool
disyuncion   []   = False
disyuncion (x:xs) = x || disyuncion xs 

aplanar :: [[a]] -> [a]
aplanar   []   = []
aplanar (x:xs) = x ++ aplanar xs     

pertenece :: Eq a => a -> [a] -> Bool
pertenece e   []   = False
pertenece e (x:xs) = e == x || pertenece e xs     

--Preguntar
apariciones :: Eq a => a -> [a] -> Int
apariciones e   []   = 0
apariciones e (x:xs) = unoSi (x == e) + apariciones e xs
--Subtarea    
unoSi :: Bool -> Int
unoSi True = 1
unoSi  _   = 0

losMenoresA :: Int -> [Int] -> [Int]
losMenoresA n   []   = []
losMenoresA n (x:xs) = if n > x 
                          then x : losMenoresA n xs
                          else losMenoresA n xs
                        
lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA n   []   = []
lasDeLongitudMayorA n (x:xs) = if longitud x > n 
                                  then x : lasDeLongitudMayorA n xs
                                  else lasDeLongitudMayorA n xs

agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal   []   y = y : []
agregarAlFinal (x:xs) y = x : agregarAlFinal xs y

agregar :: [a] -> [a] -> [a]
agregar   []     []   = []
agregar (x:xs)   l2   = x : agregar xs l2
agregar   l1   (y:ys) = y : agregar l1 ys

reversa :: [a] ->  [a]
reversa   []   = []
reversa (x:xs) = agregarAlFinal (reversa xs) x

zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos   []     []   = [] 
zipMaximos   []   (y:ys) = y : zipMaximos [] ys
zipMaximos (x:xs)   []   = x : zipMaximos xs []
zipMaximos (x:xs) (y:ys) = maxDelPar (x,y) : zipMaximos xs ys

maxDelPar :: (Int,Int) -> Int
maxDelPar (n, m) = if (n > m)
                       then n
                       else m

elMinimo :: Ord a => [a] -> a
elMinimo   []   = error "La lista está vacía"
elMinimo   [x]  = x
elMinimo (x:xs) = min x (elMinimo xs)

--2. Recursión sobre números

factorial :: Int -> Int --Precondicion: n debe ser un numero natural
factorial 0 = 1
factorial n = n * factorial (n-1)

cuentaRegresiva :: Int -> [Int] --Precondicion: n debe ser un numero natural
cuentaRegresiva 0 = []
cuentaRegresiva n = n : cuentaRegresiva (n-1)

repetir :: Int -> a -> [a]
repetir 0 e = [] 
repetir n e = e : repetir (n-1) e

losPrimeros :: Int -> [a] -> [a]
losPrimeros 0   _    = []
losPrimeros _   []   = []
losPrimeros n (x:xs) = x : losPrimeros (n-1) xs 

sinLosPrimeros :: Int -> [a] -> [a]
sinLosPrimeros 0   xs   = xs
sinLosPrimeros _   []   = [] 
sinLosPrimeros n (_:xs) = sinLosPrimeros (n-1) xs

--3. Registros
{-
1. Definir el tipo de dato Persona, como un nombre y la edad de la persona. Realizar las
siguientes funciones:
-}

data Persona = P String Int -- Nombre Edad
     deriving Show

mayoresA :: Int -> [Persona] -> [Persona]
mayoresA _ [] = []
mayoresA n (x:xs) = if edad x > n 
                       then x : mayoresA n xs 
                       else mayoresA n xs

edad :: Persona -> Int
edad (P _ e) = e 

promedioEdad :: [Persona] -> Int -- Precondición: la lista al menos posee una persona.
promedioEdad [] = error "Debe existir al menos una persona"
promedioEdad xs = div (edadesSumadas xs) (longitud xs)

edadesSumadas :: [Persona] -> Int 
edadesSumadas   []   = 0
edadesSumadas (x:xs) = edad x + edadesSumadas xs
   
elMasViejo :: [Persona] -> Persona -- Precondición: la lista al menos posee una persona.
elMasViejo   []   = error "Debe existir al menos una persona"
elMasViejo  [x]   = x
elMasViejo (x:xs) = masViejo x (elMasViejo xs)

masViejo :: Persona -> Persona -> Persona
masViejo p1 p2 = if edad p1 > edad p2 
                    then p1
                    else p2

{-
2.Como puede observarse, ahora los entrenadores tienen una cantidad de Pokemon arbitraria.
Definir en base a esa representación las siguientes funciones:
Ej: (E "Ash" [(Poke Agua 15), (Poke Fuego 100), (Poke Planta 75)])
-}

data Pokemon = Poke TipoDePokemon Int  --TipoDePokemon PorcentajeDeEnergía
             deriving Show

data TipoDePokemon = Agua | Fuego | Planta
                   deriving Show

data Entrenador = E String  [Pokemon] -- Nombre Pokemones
                deriving Show
------------------------------------------------------------------------------------

cantPokemon :: Entrenador -> Int
cantPokemon (E _ ps) = longitud ps

------------------------------------------------------------------------------------

cantPokemonDe :: TipoDePokemon -> Entrenador -> Int
cantPokemonDe tp e = cantPokemonEn tp (pokemonesDe e)  

pokemonesDe :: Entrenador -> [Pokemon]
pokemonesDe (E _ ps) = ps

cantPokemonEn :: TipoDePokemon -> [Pokemon] -> Int
cantPokemonEn tp   []   = 0
cantPokemonEn tp (p:ps) = unoSi (pokemonEsDeTipo tp p) + cantPokemonEn tp ps

pokemonEsDeTipo :: TipoDePokemon -> Pokemon -> Bool
pokemonEsDeTipo tp p1 = esMismoTipo tp (tipoDePokemon p1)

esMismoTipo :: TipoDePokemon -> TipoDePokemon -> Bool
esMismoTipo Agua   Agua   = True
esMismoTipo Fuego  Fuego  = True
esMismoTipo Planta Planta = True
esMismoTipo   _      _    = False

tipoDePokemon :: Pokemon -> TipoDePokemon
tipoDePokemon (Poke tp _) = tp

------------------------------------------------------------------------------------
-- cuantosDeTipo_De_LeGananATodosLosDe_ Fuego (E "Ash" [(Poke Fuego 54), (Poke Fuego 54), (Poke Planta 54)]) (E "Brook" [(Poke Planta 54), (Poke Planta 54), (Poke Planta 54)]) 

cuantosDeTipo_De_LeGananATodosLosDe_ :: TipoDePokemon -> Entrenador -> Entrenador -> Int 
cuantosDeTipo_De_LeGananATodosLosDe_ tp e1 e2 = cuantosDeTipo_En_LeGananATodosEn_ tp (pokemonesDe e1) (pokemonesDe e2)

cuantosDeTipo_En_LeGananATodosEn_ :: TipoDePokemon -> [Pokemon] -> [Pokemon] -> Int
cuantosDeTipo_En_LeGananATodosEn_ _     []    _   = 0
cuantosDeTipo_En_LeGananATodosEn_ tp (p1:ps1) ps2 = 
    unoSi(pokemonEsDeTipo tp p1 && leGanaATodosLosDe p1 ps2) + cuantosDeTipo_En_LeGananATodosEn_ tp ps1 ps2

leGanaATodosLosDe :: Pokemon -> [Pokemon] -> Bool
leGanaATodosLosDe p1    []    = True
leGanaATodosLosDe p1 (p2:ps2) = superaA p1 p2 && leGanaATodosLosDe p1 ps2

superaA :: Pokemon -> Pokemon -> Bool
superaA p1 p2 = esMejorTipo (tipoDePokemon p1) (tipoDePokemon p2)

esMejorTipo :: TipoDePokemon -> TipoDePokemon -> Bool
esMejorTipo Agua   Fuego  = True
esMejorTipo Fuego  Planta = True
esMejorTipo Planta Agua   = True
esMejorTipo  _      _     = False

------------------------------------------------------------------------------------

esMaestroPokemon :: Entrenador -> Bool
esMaestroPokemon e = existePokeDeTipo (pokemonesDe e) Agua   &&
                     existePokeDeTipo (pokemonesDe e) Fuego  &&
                     existePokeDeTipo (pokemonesDe e) Planta 

existePokeDeTipo :: [Pokemon] -> TipoDePokemon -> Bool
existePokeDeTipo   []   tp = False
existePokeDeTipo (p:ps) tp = pokemonEsDeTipo tp p || existePokeDeTipo ps tp                     

{-3. El tipo de dato Rol representa los roles (desarollo o management) de empleados IT dentro
de una empresa de software, junto al proyecto en el que se encuentran. Así, una empresa es
una lista de personas con diferente rol. La definición es la siguiente:
-}

data Seniority = Junior | SemiSenior | Senior
                 deriving Show

data Proyecto = Pro String
                deriving Show

data Rol = Developer Seniority Proyecto | Management Seniority Proyecto
           deriving Show

data Empresa = Emp [Rol]
               deriving Show

------------------------------------------------------------------------------------
-- (Emp [Developer Senior (Pro "Pagina Web"),Management Junior (Pro "Servicio"), Developer SemiSenior (Pro "DataBase")])

proyectos :: Empresa -> [Proyecto]
proyectos (Emp rs) = proyectosEnLista rs

proyectosEnLista :: [Rol] -> [Proyecto]
proyectosEnLista   []   = []
proyectosEnLista (r:rs) = proyecto r : proyectosEnLista rs

proyecto :: Rol -> Proyecto
proyecto (Developer _ pd)  = pd
proyecto (Management _ pm) = pm

rolesDeEmpresa :: Empresa -> [Rol]
rolesDeEmpresa (Emp r) = r

------------------------------------------------------------------------------------
--losDevSenior (Emp [Developer Senior (Pro "Pagina Web"), Developer Junior (Pro "Servicio"), Developer Senior (Pro "DataBase")]) [(Pro "Pagina Web"),(Pro "DataBase")]

losDevSenior :: Empresa -> [Proyecto] -> Int
losDevSenior (Emp rs) ps = losDevSeniorEnRoles rs ps

losDevSeniorEnRoles :: [Rol] -> [Proyecto] -> Int
losDevSeniorEnRoles   []   _  = 0
losDevSeniorEnRoles (r:rs) ps = unoSi(esDevSenior r && proyectoDe_PerteneceA r ps) + losDevSeniorEnRoles rs ps

esDevSenior :: Rol -> Bool
esDevSenior  (Developer s _ ) = esSenior s
esDevSenior         _         = False

esSenior :: Seniority -> Bool
esSenior Senior = True
esSenior   _    = False

proyectoDe_PerteneceA :: Rol -> [Proyecto] -> Bool
proyectoDe_PerteneceA (Developer  _  p) ps = estaProyectoEnLista p ps   
proyectoDe_PerteneceA (Management _  p) ps = estaProyectoEnLista p ps

estaProyectoEnLista :: Proyecto -> [Proyecto] -> Bool
estaProyectoEnLista p1    []    = False
estaProyectoEnLista p1 (p2:ps2) = nombreProyecto p1 == nombreProyecto p2 || estaProyectoEnLista p1 ps2

nombreProyecto :: Proyecto -> String
nombreProyecto (Pro s) = s

------------------------------------------------------------------------------------
--cantQueTrabajanEn [(Pro "Pagina Web")] (Emp [Developer Senior (Pro "Pagina Web"), Management Junior (Pro "Servicio"), Developer Senior (Pro "DataBase")]) 

cantQueTrabajanEn :: [Proyecto] -> Empresa -> Int
cantQueTrabajanEn ps (Emp rs) = cantDeRolesQueTrabajanEn ps rs

cantDeRolesQueTrabajanEn :: [Proyecto] -> [Rol] -> Int
cantDeRolesQueTrabajanEn _    []   = 0
cantDeRolesQueTrabajanEn ps (r:rs) = unoSi(proyectoDe_PerteneceA r ps)  + cantDeRolesQueTrabajanEn ps rs

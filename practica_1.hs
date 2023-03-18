{- 2. Números enteros -}

-- 1. Defina las siguientes funciones:
sucesor :: Int -> Int
sucesor n = n + 1

sumar :: Int -> Int -> Int
sumar n m = n + m

divisionYResto :: Int -> Int -> (Int, Int)
divisionYResto n m = (div n  m, mod n m )

maxDelPar :: (Int,Int) -> Int
maxDelPar (n, m) = if (n > m)
                       then n
                       else m

-- {-2. De 4 ejemplos de expresiones diferentes que denoten el número 10, utilizando en cada expresión a todas las funciones del punto anterior.-}

-- maxDelPar (divisionYResto (sumar 5 5) (sucesor 0))
-- sumar (sucesor 5) (maxDelPar (divisionYResto 8 2))
-- sucesor  (maxDelPar (sumar 4 5 , maxDelPar (divisionYResto 4 2)))
-- maxDelPar (7 , sumar (sucesor 3) (maxDelPar (divisionYResto 6 1)))

{-3. Tipos enumerativos-}

--1. Definir el tipo de dato Dir, con las alternativas Norte, Sur, Este y Oeste. Luego implementar las siguientes funciones:

data Dir = Norte | Este | Sur | Oeste
           deriving Show

opuesto :: Dir -> Dir
opuesto Norte = Sur
opuesto Este = Oeste
opuesto Sur = Norte
opuesto Oeste = Este

iguales :: Dir -> Dir -> Bool
iguales Norte Norte = True
iguales Sur   Sur   = True
iguales Oeste Oeste = True
iguales Este  Este  = True
iguales  _     _    = False

siguiente :: Dir -> Dir
-- Dir debe ser distinta a Oeste
siguiente Norte = Este
siguiente Este = Sur
siguiente Sur = Oeste
siguiente Oeste = error "Oeste no tiene siguiente direccion"

{- 2. Definir el tipo de dato DiaDeSemana, con las alternativas Lunes, Martes, Miércoles, Jueves,
Viernes, Sabado y Domingo. Supongamos que el primer día de la semana es lunes, y el último
es domingo. Luego implementar las siguientes funciones: -}

data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo
                 deriving Show

primerDia :: DiaDeSemana
primerDia = Lunes

ultimoDia :: DiaDeSemana
ultimoDia = Domingo

primeroYUltimoDia :: (DiaDeSemana, DiaDeSemana)
primeroYUltimoDia = (primerDia, ultimoDia)

empiezaConM :: DiaDeSemana -> Bool
empiezaConM Martes    = True
empiezaConM Miercoles = True
empiezaConM     _     = False

vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues dia1 dia2 = numeroDia dia1 >  numeroDia dia2

numeroDia :: DiaDeSemana -> Int
numeroDia Lunes     = 1
numeroDia Martes    = 2
numeroDia Miercoles = 3
numeroDia Jueves    = 4
numeroDia Viernes   = 5
numeroDia Sabado    = 6
numeroDia Domingo   = 7 

--Opcion sin enumerar dias
vieneDespues' :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues' _  Domingo   = False
vieneDespues' Domingo  _   = True
vieneDespues' _ Sabado     = False
vieneDespues' Sabado _     = True
vieneDespues' _ Viernes    = False
vieneDespues' Viernes _    = True
vieneDespues' _ Jueves     = False
vieneDespues' Jueves _     = True
vieneDespues' _ Miercoles  = False
vieneDespues' Miercoles _  = True
vieneDespues' Martes Lunes = True
vieneDespues' _      _     = False

estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio Lunes   = False
estaEnElMedio Domingo = False
estaEnElMedio    _    = True

{-3. Los booleanos también son un tipo de enumerativo. Un booleano es True o False. Defina
las siguientes funciones utilizando pattern matching (no usar las funciones sobre booleanos
ya definidas en Haskell):-}

negar :: Bool -> Bool
negar False = True
negar True = False

implica :: Bool -> Bool -> Bool
implica True False = False
implica  _     _   = True

yTambien :: Bool -> Bool -> Bool
yTambien True  True = True
yTambien False   _  = False

oBien :: Bool -> Bool -> Bool
oBien False False = False
oBien   _     _   = True


{-4. Registros-}

{-1. Definir el tipo de dato Persona, como un nombre y la edad de la persona. Realizar las
siguientes funciones:-}

data Persona = P String Int -- Nombre Edad
               deriving Show 


nombre :: Persona -> String
nombre (P n _) = n

edad :: Persona -> Int
edad (P _ e) = e

crecer :: Persona -> Persona
crecer (P n e)  = (P n (sucesor e))

cambioDeNombre :: String -> Persona -> Persona
cambioDeNombre nuevoNombre (P n e) = (P nuevoNombre e)

esMayorQueLaOtra :: Persona -> Persona -> Bool
esMayorQueLaOtra (P _ e1) (P _ e2)  = e1 > e2    

laQueEsMayor :: Persona -> Persona -> Persona
laQueEsMayor (P n1 e1) (P n2 e2) = if esMayorQueLaOtra (P n1 e1) (P n2 e2)
                                   then (P n1 e1)
                                   else (P n2 e2)

{-2. Definir los tipos de datos Pokemon, como un TipoDePokemon (agua, fuego o planta) y un
porcentaje de energía; y Entrenador, como un nombre y dos Pokémon. Luego definir las
siguientes funciones: -}

data Pokemon = Poke TipoDePokemon Int  --TipoDePokemon PorcentajeDeEnergía
             deriving Show

data TipoDePokemon = Agua | Fuego | Planta
                   deriving Show

data Entrenador = E String Pokemon Pokemon -- Nombre Pokemon_1 Pokemon_2
                deriving Show

superaA :: Pokemon -> Pokemon -> Bool
superaA (Poke Agua _)   (Poke Fuego _)  = True
superaA (Poke Fuego _)  (Poke Planta _) = True
superaA (Poke Planta _) (Poke Agua _)   = True
superaA (Poke _ _)      (Poke _ _)      = False                                                   

cantidadDePokemonDe :: TipoDePokemon -> Entrenador -> Int
cantidadDePokemonDe tp (E _ p1 p2) = unoSi (pokemonEsDeTipo tp p1) + unoSi (pokemonEsDeTipo tp p2)


unoSi :: Bool -> Int
unoSi True  = 1
unoSi False = 0

pokemonEsDeTipo :: TipoDePokemon -> Pokemon -> Bool
pokemonEsDeTipo Agua   (Poke Agua   _) = True
pokemonEsDeTipo Fuego  (Poke Fuego  _) = True
pokemonEsDeTipo Planta (Poke Planta _) = True
pokemonEsDeTipo   _           _        = False

juntarPokemon :: (Entrenador, Entrenador) -> [Pokemon]
juntarPokemon ((E _ e1p1 e1p2), (E _ e2p1 e2p2)) = e1p1 : e1p2 : e2p1 : e2p2 : []

--5. Funciones polimórficas

{-1. Defina las siguientes funciones polimórficas:-}

loMismo :: a -> a
loMismo a = a

siempreSiete :: a -> Int
siempreSiete _ = 7

swap :: (a,b) -> (b, a)
swap (a,b) = (b,a)

{-
2. Responda la siguiente pregunta: ¾Por qué estas funciones son polimórficas?
    Ya que no importa de que tipo de dato sea el argumento, siempre va a funcionar
-}

-- 6. Pattern matching sobre listas

{-1. Defina las siguientes funciones polimórcas utilizando pattern matching sobre listas (no
utilizar las funciones que ya vienen con Haskell):-}

estaVacia :: [a] -> Bool
estaVacia [] = True
estaVacia _  = False

elPrimero :: [a] -> a
elPrimero (a :_) = a

sinElPrimero :: [a] -> [a]
sinElPrimero (_ : xs) = xs

splitHead :: [a] -> (a, [a])
splitHead (a : xs) = (a, xs)

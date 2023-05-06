import PriorityQueue
import MapV1
-- ==================================================================================
-- ==================================================================================

heapSort :: Ord a => [a] -> [a]
heapSort l =priorityQueueToList (listToPriorityQueue l)

listToPriorityQueue :: Ord a => [a] -> PriorityQueue a                             
listToPriorityQueue   []   = emptyPQ
listToPriorityQueue (x:xs) = insertPQ x (listToPriorityQueue xs)

priorityQueueToList :: Ord a => PriorityQueue a -> [a]
priorityQueueToList pq = if isEmptyPQ pq 
                         then []
                         else findMinPQ pq : priorityQueueToList (deleteMinPQ pq)

-- ==================================================================================
-- ==================================================================================

map = assocM"Javier" 48
     $assocM "Ileana" 46 
     $assocM "Chiara" 23 
     $assocM "Alejo" 19 emptyM

map2 =assocM 1 11
     $assocM 2 12 
     $assocM 3 13 
     $assocM 4 14 emptyM

map3 =assocM 1 10
     $assocM 2 20 
     $assocM 5 15 
     $assocM 6 16 emptyM


--Propósito: obtiene los valores asociados a cada clave del map.
valuesM :: Eq k => Map k v -> [Maybe v]                                   
valuesM m = if null (keys m)                                              
            then []
            else let primeraKey = head (keys m) in                        
                 lookupM (primeraKey) m : valuesM (deleteM primeraKey m)  

-----------------------------------------------------------------------------------

--Propósito: indica si en el map se encuentran todas las claves dadas.
todasAsociadas :: Eq k => [k] -> Map k v -> Bool                          
todasAsociadas   []   m = True
todasAsociadas (k:ks) m = elem k (keys m) && todasAsociadas ks m 

-----------------------------------------------------------------------------------

--Propósito: convierte una lista de pares clave valor en un map.
listToMap :: Eq k => [(k, v)] -> Map k v
listToMap      []     = emptyM
listToMap ((k,v):kvs) = assocM k v (listToMap kvs)

-----------------------------------------------------------------------------------

--Propósito: convierte un map en una lista de pares clave valor
mapToList :: Eq k => Map k v -> [(k, v)]
mapToList m =  crearListaKv (keys m) m

crearListaKv :: Eq k => [k] -> Map k v -> [(k, v)]
crearListaKv   []   m = [] 
crearListaKv (k:ks) m = (k, fromJust(lookupM k m)) : crearListaKv ks m 

fromJust :: Maybe v -> v
fromJust Nothing  = error "No existe el valor"
fromJust (Just v) = v

-----------------------------------------------------------------------------------

--Propósito: dada una lista de pares clave valor, agrupa los valores de los pares que compartan la misma clave.
agruparEq :: Eq k => [(k,v)] -> Map k [v]
agruparEq kvs = listToMap (agruparL kvs)

agruparL :: Eq k => [(k,v)] -> [(k,[v])]
agruparL      []     = []
agruparL ((k,v):kvs) = let listaNueva = agruparL kvs in 
                         if pertenece k listaNueva
                         then concatenar v k listaNueva
                         else (k,[v]) : listaNueva

concatenar :: Eq k => v -> k -> [(k,[v])] -> [(k,[v])]
concatenar _ _       []     = []
concatenar v k ((x,vs):kvs) = if k == x 
                              then (x,v:vs) : kvs
                              else (x,vs) : concatenar v k kvs

pertenece ::  Eq k => k -> [(k, vs)] -> Bool
pertenece _       []    = False
pertenece c ((x,n):xns) = c == x || pertenece c xns
-----------------------------------------------------------------------------------

--Propósito: dada una lista de claves de tipo k y un map que va de k a Int, le suma uno a cada número asociado con dichas claves.
incrementar :: Eq k => [k] -> Map k Int -> Map k Int
incrementar   []   m = m
incrementar (k:ks) m = assocM k (fromJust(lookupM k m) + 1) (incrementar ks m)

-----------------------------------------------------------------------------------

--Propósito: dado dos maps se agregan las claves y valores del primer map en el segundo. Si una clave del primero existe en el segundo, es reemplazada por la del primero
mergeMaps:: Eq k => Map k v -> Map k v -> Map k v
mergeMaps m1 m2 = listToMap (mapToList m1 ++  mapToList m2)

-----------------------------------------------------------------------------------

--Propósito: dada una lista de elementos construye un map que relaciona cada elemento con su posición en la lista.
indexar ::Eq a => [a] -> Map Int a
indexar xs = indexarDesde 1 xs

indexarDesde :: Int -> [a] -> Map Int a 
indexarDesde _   []   = emptyM 
indexarDesde n (x:xs) = assocM n x (indexarDesde (n+1) xs) 

-----------------------------------------------------------------------------------

--Propósito: dado un string, devuelve un map donde las claves son los caracteres que aparecen en el string, y los valores la cantidad de veces que aparecen en el mismo
ocurrencias :: String -> Map Char Int
ocurrencias str = listToMap (listaDeApariciones str)

listaDeApariciones :: [Char] -> [(Char, Int)]
listaDeApariciones   []   = []
listaDeApariciones (c:cs) = let listaNueva = listaDeApariciones cs in 
                              if perteneceCharALista c listaNueva
                              then sumarAparicion c listaNueva
                              else (c, 1) : listaNueva

perteneceCharALista :: Char -> [(Char, Int)] -> Bool
perteneceCharALista _       []    = False
perteneceCharALista c ((x,n):xns) = c == x || perteneceCharALista c xns

--PREC: Char pertenece a la lista                                    
sumarAparicion :: Char -> [(Char, Int)] -> [(Char, Int)]
sumarAparicion c ((x,n):xns) = if c == x 
                               then (x,n+1) : xns
                               else (x,n) : sumarAparicion c xns




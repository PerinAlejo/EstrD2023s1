import MapV1
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
agruparEq :: Eq k => [(k, v)] -> Map k [v]
agruparEq      []     = emptyM
agruparEq ((k,v):kvs) = emptyM

-----------------------------------------------------------------------------------

--Propósito: dada una lista de claves de tipo k y un map que va de k a Int, le suma uno a cada número asociado con dichas claves.
incrementar :: Eq k => [k] -> Map k Int -> Map k Int
incrementar   []   m = m
incrementar (k:ks) m = assocM k (fromJust(lookupM k m) + 1) (incrementar ks m)

-----------------------------------------------------------------------------------

--Propósito: dado dos maps se agregan las claves y valores del primer map en el segundo. Si una clave del primero existe en el segundo, es reemplazada por la del primero
mergeMaps:: Eq k => Map k v -> Map k v -> Map k v


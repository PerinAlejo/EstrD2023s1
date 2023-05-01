module MapV3
    (Map, emptyM, assocM, lookupM, deleteM, keys)
where


data Map k v = M [k] [v]
{-
INV. REP.: Para M ks vs 
    - ks y vs deben tener la misma longitud
    - la clave ubicada en la posición i está asociada al valor en la misma posición, pero de la otra lista
-}

--Propósito: devuelve un map vacío
emptyM :: Map k v
emptyM = M [] []
------------------------------------------------------------------
--Propósito: agrega una asociación clave-valor al map.
assocM :: Eq k => k -> v -> Map k v -> Map k v
assocM k v (M ks vs) = 

assocLs :: Eq k => k -> v -> [k] -> [v] -> Map k v
assocLs k' v'   []     _    = 
assocLs k' v' (k:ks) (v:vs) = if k' == k 
                              then 
                              else 
------------------------------------------------------------------
--Propósito: encuentra un valor dado una clave.
lookupM :: Eq k => k -> Map k v -> Maybe v                 --O(n)
lookupM k (M ks vs) = lookupLs k ks vs

lookupLs:: Eq k => k -> [k] -> [v] -> Maybe v              --O(n)
lookupLs k'   []     _    = Nothing
lookupLs k' (k:ks) (v:vs) = if k' == k 
                            then Just v
                            else lookupLs k' ks vs 
------------------------------------------------------------------
--Propósito: borra una asociación dada una clave.
deleteM :: Eq k => k -> Map k v -> Map k v
deleteM k (M ks vs) = deleteLs k ks vs

deleteLs :: Eq k => k -> [k] -> [v] -> Map k v'
deleteLs k'   []     _    = 
deleteLs k' (k:ks) (v:vs) = if k' == k 
                            then M ks vs 
------------------------------------------------------------------
--Propósito: devuelve las claves del map
keys :: Map k v -> [k]                                     --O(1)
keys (M ks _) = ks
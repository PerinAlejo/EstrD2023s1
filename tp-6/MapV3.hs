module MapV3
    (Map, emptyM, assocM, lookupM, deleteM, keys)
where


data Map k v = M [k] [v]
        deriving Show
{-
INV. REP.: Para M ks vs 
    - ks y vs deben tener la misma longitud
    - la clave ubicada en la posición i está asociada al valor en la misma posición, pero de la otra lista
-}

map2 =assocM 1 11
     $assocM 2 12 
     $assocM 3 13 
     $assocM 4 14 emptyM

--Propósito: devuelve un map vacío
emptyM :: Map k v                                          --O(1)
emptyM = M [] []
------------------------------------------------------------------
--Propósito: agrega una asociación clave-valor al map.
-- assocM :: Eq k => k -> v -> Map k v -> Map k v             --O(n)
-- assocM k v (M ks vs) = assocKv k v ks vs 
                        
-- assocKv :: Eq k => k -> v -> [k] -> [v] -> Map k v
-- assocKv k v ks vs = if elem k ks 
--                     then M ks (reemplazarV v vs (pos k ks))--O(n)
--                     else M (k:ks) (v:vs)

-- reemplazarV :: v -> [v] -> Int -> [v]                      --O(n)
-- reemplazarV v' (v:vs) 0 = v': vs
-- reemplazarV v' (v:vs) n = v : reemplazarV v' vs (n-1) 

--PREC: k es un elemento de la lista
pos :: Eq k => k -> [k] -> Int                             --O(n)          
pos k'   []   = error "No existe k en la lista" 
pos k' (k:ks) = if k' == k 
                then 0
                else 1 + pos k' ks

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
-- --Propósito: borra una asociación dada una clave.
deleteM :: Eq k => k -> Map k v -> Map k v                 --O(n)
deleteM k (M ks vs) = deleteKv k ks vs 

deleteKv :: Eq k => k -> [k] -> [v] -> Map k v             --O(n) 
deleteKv k ks vs = if elem k ks                          
                   then let posBorrada = pos k ks in 
                    M (deletePos posBorrada ks) (deletePos posBorrada vs)
                   else M ks vs 

deletePos :: Int -> [a] -> [a]                             --O(n)
deletePos _   []   = []                             
deletePos 0 (x:xs) = xs
deletePos n (x:xs) = x : deletePos (n-1) xs
------------------------------------------------------------------
--Propósito: devuelve las claves del map
keys :: Map k v -> [k]                                     --O(1)
keys (M ks _) = ks



assocM :: Eq k => k -> v -> Map k v -> Map k v  
assocM k v (M ks vs) = let (ks', vs') = assocKv k v ks vs in (M ks' vs')

assocKv :: Eq k => k -> v -> [k] -> [v] -> ([k],[v])
assocKv k' v'   []     _    = ([k'],[v'])
assocKv k' v' (k:ks) (v:vs) = if k' == k
                              then (k:ks, v:vs)
                              else let (ks',vs') = assocKv k' v' ks vs in 
                                (k:ks',v:vs') 

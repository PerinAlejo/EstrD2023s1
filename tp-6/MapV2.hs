module MapV2
    (Map, emptyM, assocM, lookupM, deleteM, keys)
where


data Map k v = M [(k, v)]
        deriving Show

------------------------------------------------------------------
--Propósito: devuelve un map vacío
emptyM :: Map k v                                          --O(1)
emptyM = M [] 
------------------------------------------------------------------
--Propósito: agrega una asociación clave-valor al map.
assocM :: Eq k => k -> v -> Map k v -> Map k v             --O(1)
assocM k v (M kvs) = M ((k,v):kvs)
------------------------------------------------------------------
--Propósito: encuentra un valor dado una clave.
lookupM :: Eq k => k -> Map k v -> Maybe v                 --O(n)
lookupM k (M kvs) = lookupL k kvs

lookupL :: Eq k => k -> [(k,v)] -> Maybe v                 --O(n)
lookupL k'      []     = Nothing
lookupL k' ((k,v):kvs) = if k'== k 
                         then Just v
                         else lookupL k' kvs      
------------------------------------------------------------------
--Propósito: borra una asociación dada una clave.
deleteM :: Eq k => k -> Map k v -> Map k v                 --O(n)
deleteM k (M kvs) = M (deleteL k kvs)

deleteL :: Eq k => k -> [(k,v)] -> [(k,v)]                 --O(n)
deleteL k'      []     = []
deleteL k' ((k,v):kvs) = if k' == k 
                         then deleteL k' kvs 
                         else (k,v) : deleteL k' kvs
------------------------------------------------------------------
--Propósito: devuelve las claves del map.
keys :: Eq k => Map k v -> [k]                             --O(n^2)
keys (M kvs) = keysL kvs

keysL :: Eq k => [(k,v)] -> [k]                            --O(n^2)
keysL      []     = [] 
keysL ((k,v):kvs) = if elem k (keysL kvs) 
                    then keysL kvs 
                    else k : keysL kvs




module MapV1
    (Map, emptyM, assocM, lookupM, deleteM, keys)
where


data Map k v = M [(k, v)]
        deriving Show
{-
INV.REP.: Para m kvs
    -k no debe estar repetida
-}

emptyM :: Map k v                                          --O(1)
emptyM = (M [])
------------------------------------------------------------------
assocM :: Eq k => k -> v -> Map k v -> Map k v             --O(n)
assocM k v (M kvs) = (M (assocL k v kvs))

assocL :: Eq k => k -> v -> [(k,v)] -> [(k,v)]             --O(n)
assocL k v       []      = [(k,v)]
assocL k v ((k',v'):kvs) = if k == k'
                           then (k,v)  : kvs
                           else (k',v'): assocL k v kvs
------------------------------------------------------------------
lookupM :: Eq k => k -> Map k v -> Maybe v                 --O(n)
lookupM k (M kvs) = lookupL k kvs

lookupL :: Eq k => k -> [(k,v)] -> Maybe v                 --O(n)
lookupL k'      []     = Nothing
lookupL k' ((k,v):kvs) = if k'==k 
                         then Just v
                         else lookupL k' kvs      
------------------------------------------------------------------
deleteM :: Eq k => k -> Map k v -> Map k v                 --O(n)
deleteM k (M kvs) = (M (deleteL k kvs))

deleteL :: Eq k => k -> [(k,v)] -> [(k,v)]                 --O(n)
deleteL k'      []     = []
deleteL k' ((k,v):kvs) = if k' == k 
                         then kvs 
                         else (k,v) : deleteL k' kvs

------------------------------------------------------------------
keys :: Map k v -> [k]                                     --O(n)
keys (M kvs) = keysL kvs

keysL :: [(k,v)] -> [k]                                    --O(n)
keysL      []     = []
keysL ((k,v):kvs) = k : keysL kvs
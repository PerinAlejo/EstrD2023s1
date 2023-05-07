module MultiSet
    (MultiSet, emptyMS, addMS, ocurrencesMS, unionMS, intersectionMS, multiSetToList)
where

import MapV1

data MultiSet a = MS (Map a Int )
            deriving Show
{-
INV.REP.: Para MS m 
    -m debe representar un Map a Int donde Int sea la cantidad de veces que aparece a en el conjunto
-}

multiS1 = addMS 'P'(addMS 'A'(addMS 'A' emptyMS))

multiS2 = addMS 'B'(addMS 'A'(addMS 'P' emptyMS))


--Propósito: denota un multiconjunto vacío
emptyMS :: MultiSet a
emptyMS = MS emptyM

------------------------------------------------------------------
--Propósito: dados un elemento y un multiconjunto, agrega una ocurrencia de ese elemento al multiconjunto.
addMS :: Ord a => a -> MultiSet a -> MultiSet a
addMS x (MS m) = MS (incrementarAp x m) 
                   
incrementarAp :: Ord a => a -> Map a Int -> Map a Int
incrementarAp e m = listToMap(incrementarApL e (mapToList m)) 

incrementarApL :: Ord a => a -> [(a,Int)] -> [(a,Int)]
incrementarApL e      []     = [(e,1)]
incrementarApL e ((x,n):xns) = if e == x 
                                then (x,(n+1)) : xns
                                else (x,n) : (incrementarApL e xns)

listToMap :: Eq k => [(k, v)] -> Map k v
listToMap      []     = emptyM
listToMap ((k,v):kvs) = assocM k v (listToMap kvs)

mapToList :: Eq k => Map k v -> [(k, v)]
mapToList m =  crearListaKv (keys m) m

crearListaKv :: Eq k => [k] -> Map k v -> [(k, v)]
crearListaKv   []   m = [] 
crearListaKv (k:ks) m = (k, fromJust(lookupM k m)) : crearListaKv ks m 

fromJust :: Maybe v -> v
fromJust Nothing  = error "No existe el valor"
fromJust (Just v) = v

------------------------------------------------------------------
--Propósito: dados un elemento y un multiconjunto indica la cantidad de apariciones de ese elemento en el multiconjunto
ocurrencesMS :: Ord a => a -> MultiSet a -> Int
ocurrencesMS x (MS m) = apariciones (lookupM x m)

apariciones :: Maybe Int -> Int 
apariciones Nothing = 0 
apariciones (Just n)= n

------------------------------------------------------------------
--Propósito: dado un multiconjunto devuelve una lista con todos los elementos del conjunto y su cantidad de ocurrencias.
multiSetToList :: Ord a => MultiSet a -> [(a, Int)]
multiSetToList (MS m) = mapToList m 

------------------------------------------------------------------
--Propósito: dados dos multiconjuntos devuelve un multiconjunto con todos los elementos de ambos multiconjuntos
unionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a
unionMS ms1 ms2 =  MS (listToMap (unionL (multiSetToList ms1) (multiSetToList ms2)))

unionL ::  Ord a => [(a, Int)] -> [(a, Int)] -> [(a, Int)]
unionL    []    ys = ys
unionL (xn:xns) ys = unirL xn (unionL xns ys)
            
unirL :: Ord a => (a,Int) -> [(a,Int)] -> [(a,Int)]
unirL (x,n)      []     = [(x,n)]
unirL (x,n) ((y,i):yis) = if x == y 
                            then (x,n+i) : yis
                            else (y,i) : unirL (x,n) yis

------------------------------------------------------------------
--Propósito: dados dos multiconjuntos devuelve el multiconjunto de elementos que ambos multiconjuntos tienen en común.
intersectionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a
intersectionMS ms1 ms2 = MS (listToMap (intersectionL (multiSetToList ms1) (multiSetToList ms2)))

intersectionL :: Ord a => [(a,Int)] -> [(a,Int)] -> [(a,Int)]
intersectionL      []     _  = []
intersectionL ((x,n):xns) ys = if pertenece x ys 
                               then intersectarL (x,n) ys : intersectionL xns ys
                               else intersectionL xns ys

--PREC: la tupla dada debe existir en la lista
intersectarL :: Ord a => (a,Int) -> [(a,Int)] -> (a,Int)
intersectarL (x,n) ((y,i):yis) = if x == y 
                                 then (x,n+i)
                                 else intersectarL (x,n) yis

pertenece ::  Eq a => a -> [(a, Int)] -> Bool
pertenece _       []    = False
pertenece x ((y,n):yns) = x == y || pertenece x yns
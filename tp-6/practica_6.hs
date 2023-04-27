import Map 

-- ==================================================================================
-- ==================================================================================

valuesM :: Eq k => Map k v -> [Maybe v]
valuesM m = if null (keys m)
            then
            else let primeraKey = head (keys m)
                 in
                 lookupM (primeraKey) m : valuesM (deleteM primeraKey m)
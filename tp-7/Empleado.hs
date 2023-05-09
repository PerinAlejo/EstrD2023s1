module Empleado 
    (Empleado,consEmpleado,cuil,incorporarSector)
where

import SetV1

type SectorId = Int
type CUIL = Int

data Empleado = E CUIL (Set SectorId)

consEmpleado :: CUIL -> Empleado
consEmpleado c = E c emptyS

cuil :: Empleado -> CUIL
cuil (E c _) = c

incorporarSector :: SectorId -> Empleado -> Empleado
incorporarSector s (E c sId) = E c (addS s sId)


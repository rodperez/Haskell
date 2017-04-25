import Text.Show.Functions

data Cliente = Cliente {
  nombre :: Nombre,
  resistencia :: Resistencia,
  amigos :: [Cliente],
  bebidas :: [Bebida]
} deriving (Show)

type Nombre = String
type Resistencia = Double

type Bebida = Cliente -> Cliente

rodri = Cliente "Rodrigo" 150 [] []
marcos = Cliente "Marcos" 40 [rodri] [grogXD, grogXD, klusener "Huevo", klusener "Frutilla"]

bajarResistencia cuánto cliente =
  nuevaResistencia (resistencia cliente - cuánto) cliente

aumentarResistencia cuánto =
  bajarResistencia (cuánto * (-1))

afectarAmigos criterio cliente =
  cliente {
    amigos = (map criterio . amigos) cliente
  }

jarraLoca :: Bebida
jarraLoca =
  bajarResistencia 10 .
  afectarAmigos (bajarResistencia 10)

nuevaResistencia unaResistencia cliente =
  cliente {
    resistencia = unaResistencia
  }

grogXD :: Bebida
grogXD = nuevaResistencia 0

klusener :: Nombre -> Bebida
klusener nombre =
  bajarResistencia ((fromIntegral . length) nombre)

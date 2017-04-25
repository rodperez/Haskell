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

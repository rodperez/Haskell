import Text.Show.Functions
import Data.List

data Protagonista = Protagonista Nombre PuntoDeVida GenteQuerida deriving (Show, Eq)

type Nombre = String
type PuntoDeVida = Int
type GenteQuerida = [Protagonista]

--1) Modelar Protagonista.

daryl = Protagonista "Daryl" 55[carol]
maggie = Protagonista "Maggie" 100 [carol, daryl ,krilin]
carol = Protagonista "Carol" 200 [victor]
krilin = Protagonista "Krilin" 1 []
victor = Protagonista "Victor" 1 []

--2) Modelar Zombies.
nombre (Protagonista unNombre _  _)= unNombre

puntosDeVida (Protagonista _ unosPuntoDeVida _) = unosPuntoDeVida

nuevosPuntosDeVida unPuntoDeVida (Protagonista unNombre unosPuntoDeVida unaGenteQuerida ) =
  Protagonista unNombre unPuntoDeVida unaGenteQuerida

zombieTranqui :: Protagonista -> Protagonista
zombieTranqui persona = nuevosPuntosDeVida (puntosDeVida persona - 10) persona

zombieConCasco :: Protagonista -> Protagonista
zombieConCasco persona = nuevosPuntosDeVida (div (puntosDeVida persona) 2 ) persona

zombieSinDientes :: Protagonista -> Protagonista
zombieSinDientes = id
--3)
-- Mostrar en consola cómo consultarían que Carol reciba un ataque de un zombie con casco
-- *Main> zombieConCasco carol
--  Protagonista "Carol" 100

-- Mostrar en consola cómo consultarían que Carol reciba un ataque de un zombie tranqui
-- *Main> zombieTranqui carol
--  Protagonista "Carol" 190

-- ¿Cómo queda Carol luego de ambos ataques juntos?.
-- *Main> (zombieTranqui.zombieConCasco) carol
-- Protagonista "Carol" 90

--4) Modelar las formas de actuar de la forma más conveniente para poder realizar lo pedido.

type Zombie = Protagonista -> Protagonista

matar :: Zombie -> Protagonista -> Protagonista
matar tipoDeZombie  = id

caerse :: Zombie -> Protagonista -> Protagonista
caerse tipoDeZombie =  tipoDeZombie . tipoDeZombie

sacrificarse :: Zombie -> Protagonista -> Protagonista
sacrificarse tipoDeZombie  =  nuevosPuntosDeVida 0

--5) Mostrar en la consola cómo quedaría Carol si se sacrifica con un zombie tranqui.
--  *Main> sacrificarse carol zombieTranqui
--  Protagonista "Carol" 0

--6) Mostrar cómo quedaría Maggie si mata a un zombie sin dientes, y luego se cae frente a un zombie con casco.
-- *Main> ((matar zombieSinDientes).(caerse zombieConCasco)) maggie
-- Protagonista "Maggie" 25
---              SEGUNDA ENTREGA
--2)

--pelearse (Protagonista unNombre unosPuntoDeVida unaGenteQuerida) persona = Protagonista unNombre unPuntoDeVida

--quitarAmigos unProtagonista persona = filter (not(elem persona (amigos unProtagonista))  (amigos unProtagonista)

-- 3)

zombieBuenazoAstuto hambre (Protagonista unNombre unosPuntoDeVida unaGenteQuerida) | hambre < length unaGenteQuerida = zombieSinDientes (Protagonista unNombre unosPuntoDeVida unaGenteQuerida)
                                                                                   | hambre >= length unaGenteQuerida = zombieConCasco (Protagonista unNombre unosPuntoDeVida unaGenteQuerida)

zombieReSacado  = atacarAmigos . zombieTranqui

atacarAmigos (Protagonista unNombre unosPuntoDeVida unaGenteQuerida) =
   Protagonista unNombre  unosPuntoDeVida ((map zombieTranqui  .map zombieConCasco) unaGenteQuerida)

--4)

--muerto unProtagonista =   estaMuerto unProtagonista && noSonAmigos unProtagonista lista

estaMuerto (Protagonista unNombre unosPuntoDeVida unaGenteQuerida) = unosPuntoDeVida <= 0

amigos (Protagonista unNombre unosPuntoDeVida unaGenteQuerida) =  unaGenteQuerida

noSonAmigos unProtagonista lista = not(elem (head lista) (amigos unProtagonista)) && not(elem (last lista) (amigos unProtagonista))

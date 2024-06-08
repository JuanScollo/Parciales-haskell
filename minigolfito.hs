-- Modelo inicial
import Data.List
data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerzaJugador :: Int,
  precisionJugador :: Int
} deriving (Eq, Show)

-- Jugadores de ejemplo
bart :: Jugador
bart = UnJugador "Bart" "Homero" (Habilidad 25 60)
todd :: Jugador
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)
rafa :: Jugador
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)

data Tiro = UnTiro {
  velocidad :: Int,
  precision :: Int,
  altura :: Int
} deriving (Eq, Show)

type Puntos = Int

---------
--- 1 ---
---------
type Palo = Habilidad -> Tiro

putter :: Palo
putter jugador  = UnTiro{
    velocidad = 10,
    precision = precisionJugador jugador * 2,
    altura = 0
    }

madera :: Palo
madera jugador = UnTiro{
    velocidad = 100,
    precision = div (precisionJugador jugador )  2,
    altura = 5
}

hierro :: Int -> Palo
hierro n jugador = UnTiro{
    velocidad = fuerzaJugador jugador * n,
    precision = div (precisionJugador jugador)  n,
    altura = max 0 (n-3)
}

losPalos :: [Palo]
losPalos = [putter, madera] ++ map hierro [1..10]

---------
--- 2 ---
---------

golpe :: Jugador -> Palo -> Tiro
golpe jugador palo = palo . habilidad  $ jugador


---------
--- 3 ---
---------

type Obstaculo = Tiro -> (Tiro, Superado)
type Superado = Bool
tunelConRampita :: Obstaculo
tunelConRampita tiro 
    |precision tiro > 90 = (efectoTunelSuperado tiro, True)
    |otherwise = (efectoDeObstaculoNoSuperado tiro, False)

efectoTunelSuperado :: Tiro -> Tiro
efectoTunelSuperado tiro = tiro {
    velocidad = velocidad tiro * 2,
    precision = 100,
    altura = 0
}

laguna :: Int -> Obstaculo
laguna largoLaguna tiro 
    |velocidad tiro > 80 && between 1 (altura tiro) 5 = (efectoDeLagunaSuperada largoLaguna tiro, True)
    |otherwise = (efectoDeObstaculoNoSuperado tiro, False)
efectoDeLagunaSuperada :: Int -> Tiro -> Tiro
efectoDeLagunaSuperada largoLaguna tiro = tiro{
    altura = div (altura tiro) largoLaguna
}

hoyo :: Obstaculo
hoyo tiro 
    |between 5 (velocidad tiro) 20 && vaRasDeSuelo tiro && precision tiro > 95 =(efectoDeHoyoSuperado tiro, True)
    |otherwise = (efectoDeObstaculoNoSuperado tiro, False)

efectoDeHoyoSuperado :: Tiro -> Tiro
efectoDeHoyoSuperado tiro = tiro{
    velocidad = 0,
    precision = 0,
    altura = 0
}

vaRasDeSuelo :: Tiro -> Bool
vaRasDeSuelo = (== 0) . altura

efectoDeObstaculoNoSuperado :: Tiro -> Tiro
efectoDeObstaculoNoSuperado = efectoDeHoyoSuperado

---------
--- 4 ---
---------

palosUtiles :: Jugador -> Obstaculo -> [Palo]
palosUtiles jugador obstaculo = filter (paloUtil jugador obstaculo) losPalos

paloUtil :: Jugador -> Obstaculo -> Palo -> Bool
paloUtil jugador obstaculo palo = superado obstaculo (golpe jugador palo)

superado :: Obstaculo -> Tiro -> Bool
superado obstaculo tiro = snd (obstaculo tiro)

type Obstaculos = [Obstaculo]

consecutivosSuperados :: Tiro -> Obstaculos -> Int
consecutivosSuperados tiro obstaculos = length  (takeWhile (== True) (obstaculosSuperados tiro obstaculos))

obstaculosSuperados :: Tiro -> Obstaculos -> [Bool]
obstaculosSuperados _ [] = []
obstaculosSuperados tiro (obstaculo : obstaculos) = snd (obstaculo tiro) : obstaculosSuperados (fst (obstaculo tiro)) obstaculos


paloMasUtil :: Jugador -> Obstaculos -> Palo
paloMasUtil jugador obstaculos = maximoSegun (flip consecutivosSuperados obstaculos .  golpe jugador) losPalos 





-- type Obstaculo = Tiro -> (Tiro, Superado)
-- type Superado = Bool

tirito :: Tiro
tirito = UnTiro{
    velocidad = 10,
    precision = 95,
    altura = 0
}


ganador :: [(Jugador, Puntos)] -> (Jugador, Puntos)
ganador jugadores = maximumBy (mayorSegun snd) jugadores

puntosGanador :: [(Jugador, Puntos)] -> Puntos
puntosGanador jugadores = snd (ganador jugadores)

noEsGanador :: Puntos -> (Jugador, Puntos) -> Bool
noEsGanador puntosGanador (_, puntos) = puntos /= puntosGanador

jugadoresPerdedores :: [(Jugador, Puntos)] -> [(Jugador, Puntos)]
jugadoresPerdedores jugadores = filter (noEsGanador (puntosGanador jugadores)) jugadores

padresQuePierdenLaApuesta :: [(Jugador, Puntos)] -> [String]
padresQuePierdenLaApuesta jugadores = map (padre . fst) (jugadoresPerdedores jugadores)










-- Funciones útiles
between :: (Eq a, Enum a) => a -> a -> a -> Bool
between n m x = elem x [n .. m]

maximoSegun :: (Foldable t, Ord a1) => (a2 -> a1) -> t a2 -> a2
maximoSegun f = foldl1 (mayorSegun f)
mayorSegun :: Ord a => (t -> a) -> t -> t -> t
mayorSegun f a b
  | f a > f b = a
  | otherwise = b

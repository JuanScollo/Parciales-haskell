--1

data Guantelete = Guantelete {
    material :: String,
    gemas :: [Gema]
}



type Poder = Personaje -> Personaje
type Gema = Poder

data Personaje = Personaje {
    edad :: Int,
    energia :: Int,
    habilidades :: [Habilidad],
    nombre :: String,
    planeta :: Planeta
}

type Habilidad = String
type Planeta = String



data Universo = Universo{
    personajes :: [Personaje],
    energiaU :: Int
}

chasquido :: Guantelete -> Universo -> Universo
chasquido guantelete universo 
    | esGuanteleteCompleto guantelete = universo { personajes = take (numMitadDeUniverso . personajes $ universo) (personajes universo) }
    |otherwise = universo

numMitadDeUniverso :: [Personaje] -> Int
numMitadDeUniverso personajes = div (length personajes) 2

esGuanteleteCompleto :: Guantelete -> Bool
esGuanteleteCompleto guantelete = material guantelete == "uru" && (length . gemas) guantelete  == 6 

--2

aptoParaPendex :: Universo -> Bool
aptoParaPendex universo = any ((<45) . edad) (personajes universo)

energiaTotalCorrecta :: Universo -> Bool
energiaTotalCorrecta universo = energiaU universo == sumaDeEnergiaPersonajes universo

sumaDeEnergiaPersonajes :: Universo -> Int
sumaDeEnergiaPersonajes universo = sum (map energia . personajes $ universo)

--3

debilitarMente :: Int -> Gema
debilitarMente cantidad personaje = personaje { energia = max 0 (energia personaje - cantidad)}

controlarAlma :: Habilidad -> Gema
controlarAlma habilidad = debilitarMente 10 . quitarHabilidad habilidad
quitarHabilidad :: Habilidad -> Poder
quitarHabilidad habilidad personaje 
    |elem habilidad (habilidades personaje) = personaje {habilidades = filter ( /= habilidad) (habilidades personaje)}
    |otherwise = personaje


elEspacio :: Planeta -> Gema
elEspacio planeta = debilitarMente 20 . transportarA planeta
transportarA :: Planeta -> Poder
transportarA planeta personaje = personaje {planeta = planeta}

elPoder :: Gema
elPoder personaje = debilitarMente (energia personaje) . quitarSiHab2 $ personaje
quitarSiHab2 :: Poder
quitarSiHab2 personaje
    |length (habilidades personaje) < 3 = personaje { habilidades = []}
    |otherwise = personaje

elTiempo :: Gema
elTiempo = debilitarMente 50 . reducirAMitadEdad
reducirAMitadEdad :: Poder
reducirAMitadEdad personaje = personaje {edad = max 18 (div (edad personaje) 2 )}

gemaLoca :: Poder -> Gema
gemaLoca poder = poder . poder 

--4
unGuantelete :: Guantelete
unGuantelete = Guantelete {
    material = "goma",
    gemas = [elTiempo, controlarAlma "usar Mjolir",gemaLoca (controlarAlma "programacion en Haskell") ]
}

--5
utilizar :: Personaje ->[Gema] -> Personaje
utilizar = foldl (flip ($))

--6
gemaMasPoderosa :: Guantelete -> Personaje -> Gema
gemaMasPoderosa guantelete personaje = gemaDeMayorPoder personaje (gemas guantelete)

gemaDeMayorPoder :: Personaje -> [Gema] -> Gema
gemaDeMayorPoder _ [gema] = gema
gemaDeMayorPoder personaje (gema1 : gema2 : gemas) 
    | (energia . gema1 $ personaje) > (energia . gema2 $ personaje) = gemaDeMayorPoder personaje (gema2:gemas)
    | (energia . gema1 $ personaje) < (energia . gema2 $ personaje) = gemaDeMayorPoder personaje (gema1:gemas)




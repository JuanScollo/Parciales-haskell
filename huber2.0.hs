--1 
data Chofer = Chofer {
    nombre :: String,
    kilometrosAuto :: Float,
    viajesQueTomo :: [Viaje],
    condicionParaTomarViaje :: Condicion
}

data Cliente = Cliente {
    nombreCliente :: String,
    dondeVive :: String
}

data Viaje = Viaje {
    dia :: Int,
    mes :: Int,
    anio :: Int,
    costo :: Float,
    cliente :: Cliente
}

type Condicion = Viaje -> Bool
--2
tomaCualquiera :: Condicion
tomaCualquiera viaje = True

viajeMayorA200 :: Condicion
viajeMayorA200 viaje = costo viaje > 200

nombreClienteNLetras :: Int -> Condicion
nombreClienteNLetras n = (==n) . length . nombreCliente . cliente 

zonaNoIncluida :: String -> Condicion
zonaNoIncluida zona = (/=zona) . dondeVive . cliente

--3
lucas :: Cliente
lucas = Cliente {
    nombreCliente = "Lucas",
    dondeVive = "Victoria"
}



daniel :: Chofer
daniel = Chofer {
    nombre = "Daniel",
    kilometrosAuto = 23.500,
    viajesQueTomo = [Viaje 20 4 2017 150 lucas],
    condicionParaTomarViaje = zonaNoIncluida "Olivos"
}


alejandra :: Chofer
alejandra = Chofer {
    nombre = "Alejandra",
    kilometrosAuto = 180000,
    viajesQueTomo = [],
    condicionParaTomarViaje = tomaCualquiera
}

--4
puedeTomarViaje :: Viaje -> Chofer -> Bool
puedeTomarViaje viaje chofer = condicionParaTomarViaje chofer viaje 

--5 
liquidacionDeUnChofer :: Chofer -> Float
liquidacionDeUnChofer = sum . map costo . viajesQueTomo

--6
realizarViaje :: Viaje -> [Chofer] -> Chofer
realizarViaje viaje = efectuarViaje viaje . head . tomarConMenosViajes . filtrarChoferes viaje 

filtrarChoferes :: Viaje -> [Chofer] -> [Chofer]
filtrarChoferes viaje  = filter (puedeTomarViaje viaje) 

numeroDeViajes :: Chofer -> Int
numeroDeViajes = length . viajesQueTomo

tomarConMenosViajes :: [Chofer] -> [Chofer]
tomarConMenosViajes [chofer] = [chofer]
tomarConMenosViajes (chofer1 : chofer2 : choferes) 
    |numeroDeViajes chofer1 < numeroDeViajes chofer2 = chofer1 : tomarConMenosViajes choferes
    |numeroDeViajes chofer1 > numeroDeViajes chofer2 = chofer2 : tomarConMenosViajes choferes

efectuarViaje :: Viaje -> Chofer -> Chofer
efectuarViaje viaje chofer = chofer {
    viajesQueTomo = viaje : viajesQueTomo chofer
}

--7
nito :: Chofer
nito = Chofer{
    nombre = "Nito Infy",
    kilometrosAuto = 70000,
    viajesQueTomo = viajeInfinito,
    condicionParaTomarViaje = nombreClienteNLetras 3
}

repetirViaje :: Viaje -> [Viaje]
repetirViaje viaje = viaje : repetirViaje viaje

viajeInfinito :: [Viaje]
viajeInfinito = repetirViaje (Viaje 11 3 2017 50 lucas)

-- No podra calcular la liquidacion porque agarra una lista infinita y se quedara sumando infinitamente
-- Si, porque cumple con la condicion
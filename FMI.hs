data Pais = Pais {
    nombre :: String,
    ingreso :: Float,
    publicoActivo :: Float,
    privadoActivo :: Float,
    riqueza :: [String],
    deuda :: Float
} deriving(Show, Eq)

namibia :: Pais
namibia = Pais {
    nombre = "Namibia",
    ingreso = 4140,
    publicoActivo = 400000,
    privadoActivo = 650000,
    riqueza = ["mineria", "ecoturismo"],
    deuda = 50
}

type Estrategia = Pais -> Pais


e1 :: Float -> Estrategia 
e1 = prestar

prestar :: Float -> Estrategia
prestar n pais = pais{deuda = deuda pais + n * 1.5}

e2 :: Float -> Estrategia
e2 x = reducirPublico x . reducirIngreso x

reducirPublico :: Float -> Estrategia
reducirPublico x pais = pais{publicoActivo = publicoActivo pais - x}

reducirIngreso :: Float -> Estrategia
reducirIngreso x pais
    |x > 100 = pais {ingreso = ingreso pais * 0.8}
    |otherwise = pais { ingreso = ingreso pais * 0.85}

e3 :: String -> Estrategia
e3 recurso = disminuir2MDeuda . quitarRecurso recurso

disminuir2MDeuda :: Estrategia 
disminuir2MDeuda pais = pais {deuda = deuda pais - 2}

quitarRecurso :: String -> Estrategia
quitarRecurso recurso pais = pais {riqueza = filter (/= recurso) (riqueza pais) }


e4 :: Estrategia
e4 pais = prestar (pBI pais) . reducirPublico 500 $ pais 

pBI :: Pais -> Float
pBI pais = ingreso pais * (publicoActivo pais + privadoActivo pais)


puedeZafar :: [Pais] -> [Pais]
puedeZafar = filter (elem "Petroleo" . riqueza)

totalDeudaFMI :: [Pais] -> Float
totalDeudaFMI = sum . map deuda

-- composicion en los .
-- orden superior con el filter 
-- aplicacion parcial no lo hice.Applicative

-- totalDeuda :: [Pais] -> Number
-- totalDeuda = foldr ((+) . deuda) 0
--aca hay aplicacion parcial + se aplica parcialmente a deuda

--5
estrategiasOrdenadas :: Pais -> [Estrategia] -> Bool
estrategiasOrdenadas pais estrategias = estaOrdenada . map pBI $ estrategiasAplicadas pais estrategias

estrategiasAplicadas :: Pais -> [Estrategia] -> [Pais]
estrategiasAplicadas pais [] = [pais]
estrategiasAplicadas pais (estrategia : estrategias) = estrategia pais : estrategiasAplicadas (estrategia pais) estrategias

estaOrdenada :: [Float] -> Bool
estaOrdenada [] = True
estaOrdenada [_] = True
estaOrdenada (e1 : e2 : es) = e1 <= e2 && estaOrdenada (e2 : es)  


--6
--a va a revisar todos los elementos de la lista infinita, osea que no va a terminar
--b evaluara normalmente, ya que el que tenga inifinitos en riquza no cambia nada.

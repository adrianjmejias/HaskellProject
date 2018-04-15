


walkingKList = [(0,1),(0,-1),(1,0),(-1,0)]

data Casilla = Obstaculo Int
data Laberinto = Posicion Casilla Int Int

getE 1 = (742, 158) --(hp, DMG)
getE 2 = (954, 190) --(hp, DMG)
getE 3 = (1672, 110) --(hp, DMG)
getE 4 = (1408, 300) --(hp, DMG)
getE 6 = (2000, 500) --(hp, DMG)

generar_laberinto :: ([Laberinto], Int, Int) -> [[Int]]
generar_laberinto lab x y = find lab : laberinto

    where find lab x y = lab

-- resolver_laberinto XINIT YINIT XFIN YFIN direccionesList laberinto actSol BestSol = 
-- if actSol < bestSol 
-- then 0
-- else do
--     case (let estadoActual = caminar laberinto) of 
--         1 -> pelea estadoActual "hp" 

--     where 
        




-- solucion_max_salud = resolver_laberinto 0 0 maxX
-- iSolucion laberinto = 

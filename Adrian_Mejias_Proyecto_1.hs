


walkingKList = [(0,1),(0,-1),(1,0),(-1,0)]
data Punto = Punto (Int, Int)
data Casilla = Obstaculo Int
data Laberinto = Posicion Casilla Int Int

--precalculo los daños por pelea
--el daño por pelea es (ceil(div hp 500 )-1) * dmg
peleaE:: Int -> Int -> Int
peleaE 1 myHp = myHp - 158-- (742, 158) --(hp, DMG)
peleaE 2 myHp = myHp - 190-- (954, 190) --(hp, DMG)
peleaE 3 myHp = myHp - 330-- (1672, 110) --(hp, DMG)
peleaE 4 myHp = myHp - 600 --(1408, 300) --(hp, DMG)
peleaE 18 myHp = myHp - 9999 -- me mato si quiero caminar a donde he estado para hacer mas eficiente
peleaE 0 myHp = myHp

--Done and Tested
replace_elem_in_pos (x : xs) index newVal
        | index == 0 = newVal : xs
        | otherwise = x : replace_elem_in_pos xs (index - 1) newVal

--Done and Tested
replace_elem_in_matrix newVal m x y = replace_elem_in_pos m y (replace_elem_in_pos (m !! y) x newVal) 
markStep = replace_elem_in_matrix 18

--Done
peek_elem_in_matrix m x y = m !! y !! x

--
checkMap xmax ymax m x y
    | x < 0 || x > xmax = 0
    | y < 0 || y > ymax = 0
    | otherwise = 0--case peek_elem_in_matrix 
    


generar_laberinto :: ([Laberinto], Int, Int) -> [[Int]]
generar_laberinto lab xmax ymax = gen_lab lab xmax ymax 
    where
        gen_lab lab xmax ymax

(+-+) ::Num a =>(a,a) -> (a,a) -> (a,a)
(+-+) a b = (fst a + fst b, snd a + snd b)

{-
solucion_max_salud :: [[Int]] -> Int
solucion_max_salud m = g_s m   
where
    g_s m = gen_sol (Punto 0 0) (length (head m)) (length m) (length (head m)) (length m)  500 -- curryfico
    
    gen_sol init maxx maxy goalx goalyy dmg map hp actPos = 
        maximum[
            (let nextPos = actPos +-+ nextDir;) solucion_max_salud
            | nextDir <- direcciones
            , isValid nextPos
            ]
            where
                direcciones = [(1,0),(0,-1),(-1,0),(0,1)]
                
                iSolucion laberinto = solucion_max_salud > 0
                -}
                
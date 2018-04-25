


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
peleaE 6 myHp = myHp

--Done and Tested
replace_elem_in_pos (x : xs) index newVal
        | index == 0 = newVal : xs
        | otherwise = x : replace_elem_in_pos xs (index - 1) newVal

--Done and Tested
replace_elem_in_matrix ::(Num a) => a -> [[a]] -> Int -> Int -> [[a]]
replace_elem_in_matrix newVal m x y = replace_elem_in_pos m y (replace_elem_in_pos (m !! y) x newVal) 

--Done and Tested
peek_elem_in_matrix m x y = m !! y !! x

check_limits max pos
    | fst pos < 0 || fst pos >= fst max = False
    | snd pos < 0 || snd pos >= snd max = False
    | otherwise = True

(+-+) ::Num a =>(a,a) -> (a,a) -> (a,a)
(+-+) a b = (fst a + fst b, snd a + snd b)


solucion_max_salud :: [[Int]] -> Int
solucion_max_salud m = g_s 2000 (0, 0) m where 
    g_s = gen_sol ((length (head m)), (length m)) ((length (head m)) -1, (length m) -1)  500 --currifico
   -- gen_sol :: (Int,Int)->(Int,Int)->Int->Int->(Int,Int)->[[Int]] ->Int
    gen_sol max goal dmg hp actPos map = do 
        maximum[
            --Hago una lista de HPs 
            case peeked of _
                                | peeked == goal -> hp 
                                | peeked != bridge -> let myHP = (peleaE peeked hp) in if myHP <=0 then myHP else g_s myHP bridgeMove m_map
                                | otherwise -> do
                                        let bridgeMove = nextPos +-+ nextDir
                                        let landingPeek =  if is_valid_step bridgeMove 
                                            then peek_elem_in_matrix m_map (fst bridgeMove) (snd bridgeMove) 
                                            else 18 -- 18 es una casilla por la cual ya pasamos y con esto matamos el branch              
                                            case landingPeek of _ 
                                                                    | (landingPeek == goal) -> hp 
                                                                    | (landingPeek == bridge) -> 0 
                                                                    | otherwise -> 3--let myHP = (peleaE landingPeek hp) in if myHP <=0 then myHP else g_s myHP bridgeMove m_map
            --| nextDir <- direcciones
            | let nextPos = actPos +-+ nextDir
            , is_valid_step nextPos
            , let peeked = peek_elem_in_matrix m_map (fst nextPos) (snd nextPos) 
             
            ]
            where 
                is_valid_step = check_limits max
                m_map = replace_elem_in_matrix 18 map (fst actPos) (snd actPos)
                direcciones = [(1,0),(0,-1),(-1,0),(0,1)]
                goal = 6
                bridge = 5



iSolucion laberinto m = solucion_max_salud m > 0
                
                
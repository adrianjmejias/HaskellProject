rm_count z xs = let outxs = [x | x <- xs, x /= z] 
    in (outxs, count xs - count outxs) 
    where count [] = 0; count (_: zs) = 1 + count zs;


aelem e [] = False
aelem e (x : xs)
    | e == x = True
    | otherwise = aelem e xs

alast [] = error "tas meando fuera del perol"
alast [x] = x
alast (x : xs) = alast xs

ahead [] = []
ahead (x : xs) = x

noDoble [] = []
noDoble [x] = [x]
noDoble (x : xs) 
    | x == head xs = noDoble xs
    | otherwise = x : noDoble xs

    
sort::(t->t->Bool)->[t]->[t]

--Done
sort cmp [] = []
sort cmp (x:xs) = insertar cmp x (sort cmp xs) 
    where   insertar cmp z [] = [z]; 
            insertar cmp z (x : xs) | x `cmp` z = (x : (insertar cmp z xs)) | otherwise = (z : x : xs);


--Done
posiciones e xs = findWidhtIndex e xs 0
    where 
findWidhtIndex e [] index = [];
findWidhtIndex e (x : xs) index
    | e == x = index : findWidhtIndex e xs (index + 1)
    | otherwise = findWidhtIndex e xs (index +1)

--Done
elimDobles [] = []
elimDobles (x : xs) = x : elimDobles ( filter (x /=) xs)

--Done
dividir e zs = (takeWhile (<=e) zs, dropWhile (<=e) zs)

--Done but with negative nums the shit fails
insertar e [] = [e]
insertar e (y : ys)
        | e > y = y: insertar e (ys)
        | otherwise =  e : y : ys

unico xs = select xs
        where
                select [] = []
                select (x : xs)
                    | x `elem` xs = select $ filter (/= x) xs
                    | otherwise = x : select xs

numberToString m
    | up > 0 = numberToString up ++ [lo]
    | otherwise = if lo /= 0 then [lo] else []
    where
        up = m `div` 10;
        lo = m `mod` 10;

stringtoNumber ms = strToNum (reverse ms) 0
    where   strToNum (m : ms) inc = strToNum ms (inc + 1) +  m * round (10 ** inc)
            strToNum [] _ = 0


--multiplicar:: Int -> [Int] -> Int
multiplicar n ms = numberToString (n * stringtoNumber ms)
            where
                    numberToString m
                        | up > 0 = numberToString up ++ [lo]
                        | otherwise = if lo /= 0 then [lo] else []
                        where
                            up = m `div` 10;
                            lo = m `mod` 10;

                    stringtoNumber ms = strToNum (reverse ms) 0
                        where   strToNum (m : ms) inc = strToNum ms (inc + 1) +  m * round (10 ** inc)
                                strToNum [] _ = 0

intersect a b = powerIntersect (elimDobles a) (elimDobles b)
                            where powerIntersect a b = [x | x <- a, x `elem` b] 

union a b = elimDobles (a ++ b)

{-
getPals L  = [x | x <- L, preIsPal x]
where 
    preIsPal xs = isPal [numberToString x | x <- xs]  
    -}
isPal [z] = True
isPal [] = True
isPal zs 
    | head zs == last zs = isPal (tail (init zs))
    | otherwise = False

    {-
    pascal 1 = [1]
    pascal 2 = [1,1]
    pascal n = pascal 1 : pascal 2 : pacalsin (pascal 2) n 3
    where pascalsin lastPascal nPedido nAct
    | nAct == nPedido = thisPascal
    otherwise pacalsin thisPascal nPedido (nAct + 1)
    where
        thisPascal = 1 : [] ++ 1
        -}
        
m_vector_vector (v1 : vs1) (v2 : vs2) = v1 * v2 : m_vector_vector vs1 vs2
m_vector_vector [] [] = [];

m_matriz_vector m v = [sum(m_vector_vector v_m v)| v_m <-m]

get_head_col [] = []
get_head_col m = [head fila | fila <- m ]
get_tail_col [] = []
get_tail_col m = [tail fila | fila <- m]

transponer [] = []
transponer m = get_head_col m : transponer (get_tail_col m) 

m_matriz_matriz (v1 : m1) (m2) = m_vector_vector v1 (get_head_col m2) : m_matriz_matriz m1 (get_tail_col m2)
m_matriz_matriz [] [] = [] 

replace_elem_in_pos (x : xs) index newVal
        | index == 0 = newVal : xs
        | otherwise = x : replace_elem_in_pos xs (index - 1) newVal

replace_elem_in_matrix newVal m x y = replace_elem_in_pos m y (replace_elem_in_pos (m !! y) x newVal) 
marcarPaso = replace_elem_in_matrix 18
(+-+) ::Num a =>(a,a) -> (a,a) -> (a,a)
(+-+) a b = (fst a + fst b, snd a + snd b)


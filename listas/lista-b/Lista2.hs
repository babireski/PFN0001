module Lista2 where

-- 1
pertence e   []   = False
pertence e (x:xs) = if e == x then True else pertence e xs

-- 2
intercessao (x:xs) [] = []
intercessao   []   ys = []
intercessao (x:xs) ys = if pertence x ys then x : intercessao xs ys else intercessao xs ys 

-- 3
inversoLista   []   = []
inversoLista (x:xs) = (inversoLista xs) ++ [x]

-- 4
nUltimos  n   xs   = inversoLista (nUltimos' n (inversoLista xs))
nUltimos' 0 (x:xs) = []
nUltimos' n   []   = []
nUltimos' n (x:xs) = x : nUltimos' (n - 1) xs

-- 5
enesimo n   []   = -1
enesimo n (x:xs) = if n == 1 then x else enesimo (n - 1) xs

-- 6
repetir 0 m = []
repetir n m = m : repetir (n - 1) m

-- 7
intercalacao   []     []   = []
intercalacao   xs     []   = xs 
intercalacao   []     ys   = ys
intercalacao (x:xs) (y:ys) = if x < y then x : intercalacao xs (y:ys) else y : intercalacao (x:xs) ys

-- 8
menor    (x:xs) = menor' x xs
menor' n   []   = n
menor' n (x:xs) = if x < n then menor' x xs else menor' n xs

-- 9
removerElem e   []   = []
removerElem e (x:xs) = if e == x then xs else x : removerElem e xs

-- 10
ordenarLista [] = []
ordenarLista xs = menor xs : ordenarLista (removerElem (menor xs) xs)

-- 11
insereElem e   []   = [e]
insereElem e (x:xs) | e < x     = e : x : xs
                    | e > x     = x : insereElem e xs
                    | otherwise = x : xs

-- 12
primeirosDuplas []          = []    
primeirosDuplas ((x,y):xys) = x : primeirosDuplas xys

-- 13
somaDuplas []          = []
somaDuplas ((x,y):xys) = x + y : somaDuplas xys

-- 14
menoresDuplas []          = []
menoresDuplas ((x,y):xys) = if x < y then (x,y) : menoresDuplas xys else menoresDuplas xys

-- 15
separarDuplas   v   xs   = (separarDuplas' v xs, separarDuplas'' v xs)
separarDuplas'  v   []   = []
separarDuplas'  v (x:xs) = if x > v then separarDuplas'  v xs else x : separarDuplas'  v xs
separarDuplas'' v   []   = []
separarDuplas'' v (x:xs) = if x > v then x : separarDuplas'' v xs else separarDuplas'' v xs

-- 16
mdc (a,0) = a
mdc (a,b) = mdc (b,rem a b)

-- 17
inversoDupla []          = []
inversoDupla ((x,y):xys) = (y,x) : inversoDupla xys

-- 18
simetrico []          = []
simetrico ((x,y):xys) = if x == y then True : simetrico xys else False : simetrico xys

-- 19
pares i = [(x,y) | x <- [1..i], y <- [1..i], x /= y]

-- 20
inverteDNA    xs   = inversoLista (inverteDNA' xs)
inverteDNA'   []   = []
inverteDNA' (x:xs) | x == 'A' = "T" ++ inverteDNA' xs
                   | x == 'C' = "G" ++ inverteDNA' xs
                   | x == 'G' = "C" ++ inverteDNA' xs
                   | x == 'T' = "A" ++ inverteDNA' xs

-- 21
moedas = [50, 20, 10, 5]
trocoCafe  p d      = trocoCafe' (d - p) moedas
trocoCafe' 0 (x:xs) = []
trocoCafe' t   []   = []
trocoCafe' t (x:xs) = if t < x then trocoCafe' t xs else (x,(div t x)) : trocoCafe' (rem t x) xs

-- 22
magica     xs   = magica' xs ++ magica'' (inversoLista (magica' xs))
magica'    []   = []
magica'  (x:xs) = repetir (tamanho (x:xs)) x ++ magica' xs
magica'' (x:xs) = xs
tamanho   []   = 0
tamanho (x:xs) = 1 + tamanho xs
module Lista1 where

-- 1
ehTriangulo a b c = if (a > 0 && b > 0 && c > 0) && (a < b + c) && (b < a + c) && (c < a + b) then True else False

-- 2
tipoTriangulo a b c | a == b && b == c           = "equilatero"
                    | a == b || a == c || b == c = "isosceles"
                    | otherwise                  = "escaleno"

-- 3				  
triangulo a b c = if ehTriangulo a b c then tipoTriangulo a b c else "nao eh um triangulo"

-- 4
somaPares n | n > 0     = if rem n 2 == 0 then n + somaPares (n - 1) else somaPares (n - 1)
            | n < 0     = if rem n 2 == 0 then n + somaPares (n + 1) else somaPares (n + 1)
            | otherwise = 0

-- 5
somaPot2m m 0 = m
somaPot2m m n = 2 ^ n * m + somaPot2m m (n - 1)

-- 6
primo  a   = if a < 2 then False else primo' a (a - 1)
primo' a b = if b > 1 then if rem a b == 0 then False else primo' a (b - 1) else True

-- 7
seriePI    n = seriePI' 1 n
seriePI' m n = if (4 / n) < (4 / m) then 4 / m - seriePI' (m + 2) n else 0
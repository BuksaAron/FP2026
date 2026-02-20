osszeg :: Num a => a -> a -> a
osszeg a b = a + b

osszeg2 a b = (+) a b

kulombseg a b = a - b

kulombseg2 a b = (-) a b

szorzat a b = a * b

szorzat2 a b = (*) a b

hanyados a b = a / b

hanyados2 a b = a `div` b

hanyados3 a b = (div) a b

osztmar a b = mod a b

osztmar2 a b = a `mod` b

elsoF a b = (-b) / a

abszolut n = if n < 0 then -n else n

abszolut2 n
    | n < 0 = -n
    | otherwise = n 

elojel n = if n < 0 then "negativ" else if n > 0 then "pozitiv" else "nulla"

elojel2 n
    | n < 0 = "negativ"
    | n > 0 = "pozitiv"
    | otherwise = "nulla"

max1 a b = if a > b then a else b

max2 a b
    | a > b = a
    | otherwise = b

min1 a b = if a < b then a else b

min2 a b
    | a < b = a
    | otherwise = b

masodF a b c = if delta < 0 then error "komplex szam" else (gy1,gy2)
    where 
        delta = b ** 2 - 4 * a * c
        gy1 = (-b + sqrt delta) / (2 * a)
        gy2 = (-b - sqrt delta) / (2 * a)

masodF2 a b c
    | delta < 0 = error "komplex szam"
    | otherwise = (gy1,gy2)
    where 
        delta = b ** 2 - 4 * a * c
        gy1 = (-b + sqrt delta) / (2 * a)
        gy2 = (-b - sqrt delta) / (2 * a)

masodF3 a b c
    | delta < 0 = error "komplex szam"
    | delta == 0 = [gy1]
    | otherwise = [gy1,gy2]
    where 
        delta = b ** 2 - 4 * a * c
        gy1 = (-b + sqrt delta) / (2 * a)
        gy2 = (-b - sqrt delta) / (2 * a)

elempar ep1 ep2 = if (a == c && b == d) || (a == d && b == c) then True else False
    where 
        (a, b) = ep1
        (c, d) = ep2

elempar2 ep1 ep2 = (a == c && b == d) || (a == d && b == c)
    where 
        (a, b) = ep1
        (c, d) = ep2

fakt1 0 = 1
fakt1 n = n * fakt1 (n - 1)

fakt2 n
    | n < 0 = error "negativ szam"
    | n == 0 = 1
    | otherwise = n * fakt2 (n - 1)

fakt3 res n
    | n < 0 = -1
    | n == 0 = res
    | otherwise = fakt3 (res * n) (n - 1)
import Control.Monad.RWS (MonadState(put))
import Language.Haskell.TH (prim)
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

negyzetgyokN n = [sqrt i | i <- [1 .. n]]

negyzetN n = [i * i | i <- [1 .. n]]

kobN n = [i ^ 3 | i <- [1 .. n]]

nemnegyzetN n = [i | i <- [1 .. n], (sqrt i * sqrt i) /= i ]

xhatvanyN x n = [x ^ i | i <- [1 .. n]]

osztoN n = [i | i <- [1 .. n], n `mod` i == 0]

osztoN2 n = [i | i <- [2, 4 .. n], mod n i == 0]

osztok n = [i | i <- [1 .. n], n `mod` i == 0]

primszam n = osztok n == [1, n]

primszamokN n = [i | i <- [2 .. n], primszam i]

primszamokN2 n = [i | i <- [1 .. n], primszamL i]
    where
        primszamL n = osztokL n == [1, n]
        osztokL n = [i | i <- [1 .. n], n `mod` i == 0]

osszetettN n = [i | i <- [0 .. n], not (primszam i)]

paratlanosszetettN n = [i | i <- [0 .. n], not (primszam i), mod i 2 /= 0]

paratlanosszetettN2 n = [i | i <- [1, 3 .. n], not (primszam i)]

pitagorasz n = [(a, b, c) | c <- [1 .. n], b <- [1 .. c], a <- [1 .. b], a ^ 2 + b ^ 2 == c ^ 2]

betuSzam = zip ['a' .. 'z'] [0 .. 25]

szamok1 = zip[0 .. 5][5, 4 .. 0]

szamok2 n = [(i, n - i) | i <- [0 .. n]]

tfLs n = take n ls
    where 
        ls = [True, False] ++ ls

main :: IO ()
main = do
    putStrLn("x hatvany n")
    print(xhatvanyN 5 5)
    putStrLn("oszto n")
    print(osztoN 48)
    putStrLn("oszto n 2")
    print(osztoN2 48)
    putStrLn("primszamok n")
    print(primszamokN 30)
    putStrLn("primszamok n")
    print(primszamokN2 30)
    putStrLn("osszetett n")
    print(osszetettN 30)
    putStrLn("paratlan osszetett n")
    print(paratlanosszetettN 30)
    putStrLn("paratlan osszetett n 2")
    print(paratlanosszetettN2 30)
    putStrLn("pitagorasz")
    print(pitagorasz 100)
    putStrLn("5 true false: " ++ show(tfLs 5))
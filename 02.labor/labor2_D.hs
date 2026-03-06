import Graphics.Win32 (eWX_LOGOFF, nOTSRCCOPY, lBS_NOINTEGRALHEIGHT, dEFAULT_CHARSET)

-- -- # 2. labor

-- -- I. Könyvtárfüggvények használata nélkül, definiáljuk azt a függvényt, amely meghatározza:

-- -- - egy szám számjegyeinek szorzatát (2 módszerrel),
szjSzorzat n 
    | n < 0 = error "negativ szam"
    | n == 0 = 1
    | otherwise = (n `mod` 10) * szjSzorzat (n `div` 10)

szjSzorzat2 n 
    | n < 0 = error "negativ szam"
    | div n 10 == 0 = n
    | otherwise = mod n 10 * szjSzorzat2 (div n 10)

ls1 = [234, 6, 0, 64321]

szjSzorzatls :: [Int] -> [Int]
szjSzorzatls ls = map szjSzorzat2 ls

szjSzorzatls2 = map(\x -> (x, szjSzorzat2 x)) ls1
-- -- - egy szám számjegyeinek összegét (2 módszerrel),
szjOsszeg 0 = 0
szjOsszeg n = mod n 10 + szjOsszeg (n `div` 10)

szjOsszeg2 n 
    | n < 0 = error "negativ szam"
    | n == 0 = 0
    | otherwise = (n `mod` 10) + szjOsszeg (n `div` 10)

szjOsszegLs ls = map szjOsszeg2 ls

szjOsszegLs2 ls = map(\x -> (x, szjOsszeg2 x)) ls

-- -- - egy szám számjegyeinek számát (2 módszerrel),
szjSzam 0 = 0
szjSzam n = 1 + szjSzam (n `div` 10)

szjSzam2 n
    | n < 0 = szjSzam2(abs n)
    | n == 0 = 0
    | otherwise = 1 + szjSzam2 (n `div` 10)

szjSzamLs ls = map szjSzam2 ls

szjSzamLs2 ls = map(\x -> (x, szjSzam2 x)) ls
-- -- - egy szám azon számjegyeinek összegét, mely paraméterként van megadva, pl. legyen a függvény neve fugv4, ekkor a következő meghívásra, a következő eredményt kell kapjuk:
-- --   ```haskell
-- --   > fugv4 577723707 7
-- --   35
-- --   ```
szamSzjOsszeg n szj
    | szj > 9 = error "nem szamjegy"
    | n < 10 = if n == szj then szj else 0
    | otherwise = if mod n 10 == szj 
        then szj + szamSzjOsszeg(div n 10) szj
        else szamSzjOsszeg (div n 10) szj

szamSzjOsszeg2 n szj elof
    | szj > 9 = error "nem szj"
    | n < 10 = if n == szj then (elof + 1) * szj else elof * szj
    |otherwise =
        if mod n 10 == szj
            then szamSzjOsszeg2(div n 10) szj (elof + 1)
            else szamSzjOsszeg2(div n 10) szj elof

ls2 = [(577723707, 7), (54, 7), (0, 0), (124522, 2)]

szamSzjOsszegLs ls = map (uncurry szamSzjOsszeg) ls

szamSzjOsszegLs2 ls = map (\(x, szj) -> szamSzjOsszeg2 x szj 0) ls

-- -- - egy szám páros számjegyeinek számát,
parosSzj n
    | n < 0 = parosSzj (abs n)
    | n < 10 = if even n then 1 else 0
    | otherwise =
        if even utolso
        then 1 + parosSzj (div n 10)
        else parosSzj (div n 10)
    where
        utolso = mod n 10

parosSzjLs = map(\x -> (x, parosSzj x))

-- -- - egy szám legnagyobb számjegyét,

lnSzj n ln
    | n < 0 = lnSzj (abs n) ln
    | n < 10 = max n ln
    | otherwise =
        if mod n 10 > ln
        then lnSzj (div n 10) (mod n 10)
        else lnSzj (div n 10) ln
        -- -- - egy szám $b$ számrendszerbeli alakjában a $d$-vel egyenlő számjegyek számát (például a $b = 10$-es számrendszerben a $d = 2$-es számjegyek száma),
-- --   Példák függvényhívásokra:

-- --   ```haskell
-- --   fugv 7673573 10 7 -> 3
-- --   fugv 1024 2 1 -> 1
-- --   fugv 1023 2 1 -> 10
-- --   fugv 345281 16 4 -> 2
-- --   ```
bSzamDSzj n b d 
    | n < 0 = bSzamDSzj (abs n) b d
    | n < b = if n == d then 1 else 0
    | otherwise = if mod n b == d then 1 + bSzamDSzj(div n b) b d else bSzamDSzj(div n b) b d

ls3 = [(577723707, 10, 7), (54, 9, 7), (0, 0, 0), (124522, 2, 2)]

bSZamDSzjLs = map (\(n, b, d) -> bSzamDSzj n b d) ls3

-- -- - az 1000-ik Fibonacci számot.
fiboN n = fiboSg 0 1 0 n
    where
        fiboSg _ _ res 0 = res
        fiboSg a b res n = fiboSg b res (res + b) (n - 1)

fibo _ _ res 0 = res
fibo a b res n = fibo b res (res + b) (n - 1)

fiboN2 :: Num t => p -> t
fiboN2 n = fibo 0 1 0 1000

fiboSZamok n = map(fibo 0 1 0)[0 .. n]

fiboSzamok2 n = map(\x -> fibo 0 1 0 x)[0 .. n]

fiboN3 n = fiboSZamok n !! n

-- -- II. Alkalmazzuk a map függvényt a I.-nél megírt függvényekre.

-- -- **Megoldott feladatok:**

-- -- - Határozzuk meg egy szám számjegyeinek összegét:
-- --   I. módszer:

-- --   ```haskell
-- --   szOsszeg :: Int -> Int
-- --   szOsszeg 0 = 0
-- --   szOsszeg x = ( x `mod` 10 ) + szOsszeg (x `div` 10)

-- --   > szOsszeg 123
-- --   ```

-- --   II. módszer:

-- --   ```haskell
-- --   szOsszeg1 :: Int -> Int -> Int
-- --   szOsszeg1 0 t = t
-- --   szOsszeg1 x t = szOsszeg1 (x `div` 10) ( t + x `mod` 10 )

-- --   > szOsszeg1 123 0
-- --   ```
-- -- ``

main :: IO ()
main = do
    let fel1 = szjSzorzat 1234
    print fel1
    let fel2 = szjOsszeg 1234
    print fel2
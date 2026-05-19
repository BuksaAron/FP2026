import Data.List(sort, group, sortOn)
import Data.Char
-- 1. Egy [(String, Int)] típusú lista eleme egy városnevet és a megfelelő népesség
-- értéket tárolja. Írjunk egy Haskell függvényt, amely meghatározza, azokat a
-- városokat, amelyek népesség értéke egy adott n értéknél nagyobb. A kapott
-- városneveket ábécé sorrendbe rendezve külön sorba írjuk ki a képernyőre.
-- Például:
-- ● Bemenet: 150000 [("sepsiszentgyorgy", 54000), ("kolozsvár", 330000),
-- ("marosvasarhely", 130000), “temesvar", 310000), ("arad", 160000),
-- ("gyergyoszentmiklos", 18000), ("nagyvarad",196000)]
-- ● Kimenet:
-- A(z) 150000 nepesseg erteknel nagyobbal rendelkezo varosok a kovetkezok:
-- - arad
-- - kolozsvár
-- - nagyvarad
-- - temesvar
-- ● Amennyiben nincs olyan város, amelyiknek a népesség értéke egy adott n
-- értéknél nagyobb, a következő a kimenet: “Nincs x erteknel nagyobb nepesseg
-- ertekkel rendelkezo varos.”

valogat :: [(String, Int)] -> Int -> [String]
valogat varosok n = sort [nev | (nev, ertek) <- varosok, ertek > n]

fel1 :: IO ()
fel1 = do
    let varosok = 
            [ ("sepsiszentgyorgy", 54000), 
              ("kolozsvár", 330000),
              ("marosvasarhely", 130000), 
              ("temesvar", 310000), 
              ("arad", 160000),
              ("gyergyoszentmiklos", 18000), 
              ("nagyvarad", 196000)
            ] 
        n = 150000
        varosNevek = valogat varosok n

    if null varosNevek
        then putStrLn $ "Nincs " ++ show n ++ " erteknel nagyobb nepesseg ertekkel rendelkezo varos."
        else do
            putStrLn $ "A(z) " ++ show n ++ " nepesseg erteknel nagyobbal rendelkezo varosok a kovetkezok:"
            mapM_ (\v -> putStrLn ("- " ++ v)) varosNevek

-- 2. Írjunk egy Haskell függvényt, amely meghatározza egy bemeneti egész
-- számokat tartalmazó lista azon elemeit, amelyek nem tartalmazzák a 0
-- számjegyet. Az eredmény számokat szóközzel elválasztva írjuk ki a
-- képernyőre.
-- Például:
-- ● Bemenet: [17603, 4005, 3223, 816252, 70, 23561, 9018007, 807, 61, 300]
-- ● Kimenet: A 0 szamjegyet nem tartalmazo szamok a kovetkezok: 3223 816252
-- 23561 61
-- ● Amennyiben nincsenek ilyen számok, a kimenet a következő: “Nincsenek
-- olyan szamok, amelyek nem tartalmazzak a 0 szamjegyet.”


nemTartalmazNullat n = '0' `notElem` (show n)

fel2 :: IO ()
fel2 = do
    let ls = [17603, 4005, 3223, 816252, 70, 23561, 9018007, 807, 61, 300]
        ls1 = filter nemTartalmazNullat ls
    
    if null ls1
        then putStrLn "Nincsenek olyan szamok, amelyek nem tartalmazzak a 0 szamjegyet."
        else do
            putStr "A 0 szamjegyet nem tartalmazo szamok a kovetkezok: "
            putStrLn (unwords (map show ls1))

-- 3. Egy listában karakterláncok vannak, írjunk egy Haskell programot, amely kiírja
-- azokat a karakterláncokat a képernyőre egymás alá rendezve ábécé
-- sorrendbe, amelyekben nincsenek számjegyek.
-- Például:

-- ● Bemenet: ["2023tuple", "function", "float", "higher-order", "variable10",
-- "may13be", "0recursion", "monad", "class"]
-- ● Kimenet:
-- A karakterlancok, amelyek nem tartalmaznak szamokat:
-- class
-- float
-- function
-- Higher-order
-- monad
nincsSZam n = not (any isDigit n)

fel3 = do
    let ls = ["2023tuple", "function", "float", "higher-order", "variable10", "may13be", "0recursion", "monad", "class"]
        ls1 = filter nincsSZam ls
    if null ls1 
        then putStrLn "Nincsenek olyan karaktererk amelyek tartalmaznak szamot"
        else do
           putStrLn "A karakterlancok, amelyek nem tartalmaznak szamokat:"
           mapM_ putStrLn (sort ls1)

-- 4. Írjunk egy Haskell programot, amely meghatározza, hogy az s karakterláncnak
-- melyek a szomszédjai az lsS karakterláncokat tartalmazó listából, ahol egy
-- karakterlánc szomszédjait az ábécé sorrend szerinti kell érteni.
-- Például:
-- ● Bemenet:
-- s = feri
-- lsS = Mari Zsuzsa szidi Lori kata feri teri Dani zsolti
-- ● Kimenet: feri baloldali szomszedja Zsuzsa, jobboldali szomszedja pedig kata

ketoldaliSZomszedok s lsS = aux (sort lsS)
    where
        aux(x : y : z : ve)
            | x == s = [y]
            | y == s = [x, z]
            | z == s = [y]
            | otherwise = aux ve

fel4 = do
    let s = "feri" 
        lsS = ["Mari", "Zsuzsa", "szidi", "Lori", "kata", "feri", "teri", "Dani", "zsolti"]
        szomszedok = ketoldaliSZomszedok s lsS
    putStrLn (s ++ " baloldali szomszedja " ++ head szomszedok ++ " jobb oldali szomszeja " ++ last szomszedok)

-- 5. Egy [(String, Int, Int)] típusú lista eleme egy telefon márkanevet, egy eladási
-- értéket, és egy árat tartalmaz. Írjunk egy Haskell programot, amely
-- meghatározza azokat a telefonokat, amelyekből a legtöbbet adtak el, illetve
-- mennyi volt ez az érték. Az eredmény márkaneveket rendezve egymás alá
-- írjuk, amelyek elé írjuk ki egy kisérő szöveggel együtt a maximális eladási
-- értéket.
-- Például:
-- ● Bemenet: [("iphoneS1", 20, 2500), ("huaweiS1", 30, 1700), ("huaweiS2", 25,
-- 3100), ("samsungA1", 30, 2000), ("nokia", 10, 1900), ("iphoneS2", 10, 2200),
-- ("samsungA2", 15, 1650), ("iphone3", 30, 1800)]
-- ● Kimenet: A maximalis eladasi ertek 30. A telefonok, amelyeknek ennyi az
-- eladasi erteke a kovetkezok:
-- - iphone3
-- - huaweiS1
-- - samsungA1

maxEladas lista = maximum [eladas | (_, eladas, _) <- lista]

legnepszerubbek lista m = sort [nev | (nev, eladas, _) <- lista, eladas == m]

fel5 = do
    let telefonok = [ ("iphoneS1", 20, 2500), 
                      ("huaweiS1", 30, 1700), 
                      ("huaweiS2", 25, 3100), 
                      ("samsungA1", 30, 2000), 
                      ("nokia", 10, 1900), 
                      ("iphoneS2", 10, 2200), 
                      ("samsungA2", 15, 1650), 
                      ("iphone3", 30, 1800)
                    ]
    
    if null telefonok
        then putStrLn "A lista ures."
        else do
            let m = maxEladas telefonok
                nyertesek = legnepszerubbek telefonok m
            
            putStrLn $ "A maximalis eladasi ertek " ++ show m ++ "."
            putStrLn "A telefonok, amelyeknek ennyi az eladasi erteke a kovetkezok:"
            mapM_ (\nev -> putStrLn ("- " ++ nev)) nyertesek

-- 6. Írj egy Haskell függvényt, melynek egy lista a bemenete, és megadja azokat a
-- számokat, amelyek előfordulási száma páratlan. Az eredményt írasd ki a
-- példában szereplő formában, előfordulási érték szerint rendezve.
-- Például:
-- ● Bemenet: [7]
-- ● Kimenet: Elofordulas: 1 -> Ertek: 7
-- ● Bemenet: [1, 1, 2]
-- ● Kimenet: Elofordulas: 1 -> Ertek: 2
-- ● Bemenet: [1, 1]
-- ● Kimenet: Nincs paratlan elofordulasi ertekkel rendelkezo szam.
-- ● Bemenet: [1, 1, 2, 3, 4, 2, 6, 2, 4, 4, 2, 6, 7, 6, 6, 2]
-- ● Kimenet:
-- Elofordulas: 1 -> Ertek: 3
-- Elofordulas: 1 -> Ertek: 7
-- Elofordulas: 3 -> Ertek: 4
-- Elofordulas: 5 -> Ertek: 2

paratlanElofordulasok :: (Ord a, Show a) => [a] -> IO ()
paratlanElofordulasok xs =
    let csoportok = group (sort xs)
        paratlanok = filter (odd . length) csoportok
        rendezett = sortOn length paratlanok
    in if null rendezett
       then putStrLn "Nincs paratlan elofordulasi ertekkel rendelkezo szam."
       else mapM_ kiir rendezett

kiir :: Show a => [a] -> IO ()
kiir csoport =
    putStrLn $ "Elofordulas: " ++ show (length csoport)
            ++ " -> Ertek: " ++ show (head csoport)

fel6 :: IO ()
fel6 = do
    paratlanElofordulasok [7]
    paratlanElofordulasok [1,1,2]
    paratlanElofordulasok [1,1]
    paratlanElofordulasok [1,1,2,3,4,2,6,2,4,4,2,6,7,6,6,2]
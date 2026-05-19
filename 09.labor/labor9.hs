import Data.Char
import Data.List(find, isPrefixOf)
import Data.Maybe()
-- # 9. labor

-- I. Formázzuk egy adott szövegállomány tartalmát a következőképpen: azok után az írásjelek után, amelyek benne vannak a $\{.,!?;\}$ halmazban szigorúan egy szóközt tegyünk, hagyjunk.

punctuation = ".,!?;"

formatText [] = []
formatText (x : xs)
    | x `elem` punctuation = x : '\n' : formatText (dropWhile isSpace xs)
    | otherwise = x : formatText xs

main :: IO()
main = do
    putStrLn "Add meg a bemenetet: "
    input <- getLine
    let output = formatText input
    putStrLn "\nA formazott szoveg: "
    putStrLn output

-- II. Az [iban.txt](https://www.ms.sapientia.ro/~mgyongyi/Funk_Log/iban.txt) állomány IBAN kódokat tartalmaz. Írjunk egy-egy Haskell függvényt, amely

-- - beolvassa, majd rendezi az állományban levő adatokat ábécé sorrendbe,
-- - bináris keresést alkalmazva ellenőrzi, hogy egy megadott IBAN kód szerepel-e az adatok között,
-- - átírja egy okIban.txt állományba azokat az IBAN kódokat, amelyek megfelelő formátumúak. Egy IBAN kód akkor tekinthető megfelelő formátumúnak
--   - ha csak számjegyeket és angol ábécébeli nagybetűket tartalmaz,
--   - ha az IBAN kód hossza megegyezik az országhoz tartozó hosszal, ahol az országhoz tartozó hosszérték az [ibanLength.txt](https://www.ms.sapientia.ro/~mgyongyi/Funk_Log/ibanLength.txt) állományból olvasható ki,
--   - ha az átcsoportosítás és a helyettesítés után kapott egész szám 97-el való osztási maradéka egyenlő eggyel, ahol
--     - átcsoportosítás: az IBAN kód első négy karakterét kitöröljük a kód elejéről és a kód végéhez fűzzük,
--     - helyettesítés:
--       - az alfanumerikus karaktereket helyettesítsük a következő kódokkal: $$A \to 10,\ B \to 11,\ \ldots,\ Z \to 35$$
--       - az így kapott karakterláncot egész számnak tekintjük

--   Például:
--   legyen az IBAN kód: $$\texttt{GB82WEST12345698765432}$$
--   - hossz: $$22$$
--   - átcsoportosítás:
--     $$\texttt{WEST12345698765432}\ \texttt{GB82}$$
--   - helyettesítés:
--     $$32142829\quad 12345698765432\quad 1611\quad 82$$
--   - ellenőrzés: $$3214282912345698765432161182 \bmod 97 = 1$$

-- III. Egy szövegállományban egy adott személyről következő adatok vannak eltárolva: vezetéknév, keresztnév, születési dátum. Hozzuk létre a következő típusú adatszerkezeteket, majd olvassuk ki az adatokat az állományból és állapítsuk meg mindegyik személyről, hogy a hét milyen napján született és mikor van a névnapja. A névnapok megállapításához használhatjuk a [névnapokat](https://www.ms.sapientia.ro/~mgyongyi/Funk_Log/nevnapok.txt) tartalmazó szövegállományt.

-- ```haskell
-- data Datum = Datum {
--   nap :: Int,
--   honap:: Int,
--   ev :: Int
-- } deriving (Show)

-- data Szemely = Szemely {
--   vnev :: [Char],
--   knev :: [Char],
--   szdatum :: Datum
-- } deriving (Show)
-- ```

data Datum = Datum {
    nap :: Int,
    honap :: Int,
    ev :: Int
} deriving (Show, Eq)

data Szemely = Szemely {
    vnev :: [Char],
    knev :: [Char],
    szdatum :: Datum
} deriving (Show)

hetNapja (Datum d m y) = 
    let (m', y') = if m < 3 then (m + 12, y - 1) else (m, y)
        k = y' `mod` 100
        j = y' `div` 100
        h = (d + ((13 * (m' + 1)) `div` 5) + k + (k `div` 4) + (j `div` 4) + (5 * j)) `mod` 7
        napok = ["Szombat", "Vasarnap", "Hetfo", "Kedd", "Szerda", "Csutortok", "Pentek"]
    in napok !! h

keresNevNap tartalom keresztnev = 
    let sorok = lines tartalom
        talalat = find (\s -> (keresztnev ++ " ") `isPrefixOf` s) sorok
    in case talalat of
        Just s -> drop (length keresztnev + 1) s
        Nothing -> "Nincs talalat"

processSzemely nevnapAdat sor = do
    let w = words sor
        szemely = Szemely (w !! 0) (w !! 1) (Datum (read (w !! 4)) (read (w !! 3)) (read (w !! 2)))
        szulNap = hetNapja (szdatum szemely)
        nNap = keresNevNap nevnapAdat (knev szemely)
    
    putStrLn $ vnev szemely ++ " " ++ knev szemely ++ ":"
    putStrLn $ " - Szuletett: " ++ szulNap
    putStrLn $ " - Nevnap: " ++ nNap

main2 :: IO()
main2 = do
    nevnapok <- readFile "09.labor/nevnapok.txt"
    szemelyContent <- readFile "09.labor/szemely.txt"
    let szemelySorok = lines szemelyContent

    mapM_ (processSzemely nevnapok) szemelySorok
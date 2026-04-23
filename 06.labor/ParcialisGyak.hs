-- Parcialis feladatok
{- HLINT ignore "Use camelCase" -}

import Data.List (sort, group)
import Data.Char (isDigit)
import Control.Monad.RWS (MonadState(put))

--1. feladat

--1. Egy [(String, Int)] típusú lista eleme egy városnevet és a megfelelő népesség
--értéket tárolja. Írjunk egy Haskell függvényt, amely meghatározza, azokat a
--városokat, amelyek népesség értéke egy adott n értéknél nagyobb. A kapott
--városneveket ábécé sorrendbe rendezve külön sorba írjuk ki a képernyőre.
--Például:
-- ● Bemenet: 150000 [("sepsiszentgyorgy", 54000), ("kolozsvár", 330000),
--("marosvasarhely", 130000), “temesvar", 310000), ("arad", 160000),
--("gyergyoszentmiklos", 18000), ("nagyvarad",196000)]
-- ● Kimenet:
-- A(z) 150000 nepesseg erteknel nagyobbal rendelkezo varosok a kovetkezok:
-- - arad
-- - kolozsvár
-- - nagyvarad
-- - temesvar
-- ● Amennyiben nincs olyan város, amelyiknek a népesség értéke egy adott n
-- értéknél nagyobb, a következő a kimenet: “Nincs x erteknel nagyobb nepesseg
-- ertekkel rendelkezo varos.”



varosokatSzur :: Int -> [(String, Int)] -> IO ()
varosokatSzur n varosok = do

    let megfeleloVarosok = [nev | (nev, nepesseg) <- varosok, nepesseg > n]

    let rendezettVarosok = sort megfeleloVarosok

    if null rendezettVarosok
        then putStrLn "Nincs x erteknel nagyobb nepesseg ertekkel rendelkezo varos."
        else do
            putStrLn $ "A(z) " ++ show n ++ " nepesseg erteknel nagyobbal rendelkezo varosok a kovetkezok:"
            mapM_ (\varos -> putStrLn $ "- " ++ varos) rendezettVarosok

fel1 = do

    --1. feladat tesztelése
    let varosok = [("sepsiszentgyorgy", 54000), ("kolozsvár", 330000),
                    ("marosvasarhely", 130000), ("temesvar", 310000),
                    ("arad", 160000), ("gyergyoszentmiklos", 18000),
                    ("nagyvarad",196000)]
    varosokatSzur 190000 varosok

--2. feladat

--2. Írjunk egy Haskell függvényt, amely meghatározza egy bemeneti egész
--számokat tartalmazó lista azon elemeit, amelyek nem tartalmazzák a 0
--számjegyet. Az eredmény számokat szóközzel elválasztva írjuk ki a
--képernyőre.
--Például:
-- ● Bemenet: [17603, 4005, 3223, 816252, 70, 23561, 9018007, 807, 61, 300]
-- ● Kimenet: A 0 szamjegyet nem tartalmazo szamok a kovetkezok: 3223 816252
--23561 61
-- ● Amennyiben nincsenek ilyen számok, a kimenet a következő: “Nincsenek
--olyan szamok, amelyek nem tartalmazzak a 0 szamjegyet.”

nullakNelkul szamok = do

    let szurtSzamok = [szam | szam <- szamok, notElem '0' (show szam)]

    if null szurtSzamok
        then putStrLn "Nincsenek olyan szamok, amelyek nem tartalmazzak a 0 szamjegyet."
        else do
            putStrLn "A 0 szamjegyet nem tartalmazo szamok a kovetkezok: "
            putStrLn $ unwords (map show szurtSzamok)

fel2 = do
    --2. feladat tesztelése
    let szamok = [17603, 4005, 3223, 816252, 70, 23561, 9018007, 807, 61, 300]
    nullakNelkul szamok

-- 3. feladat

--3. Egy listában karakterláncok vannak, írjunk egy Haskell programot, amely kiírja
--azokat a karakterláncokat a képernyőre egymás alá rendezve ábécé
--sorrendbe, amelyekben nincsenek számjegyek.
--Például:

-- ● Bemenet: ["2023tuple", "function", "float", "higher-order", "variable10",
-- "may13be", "0recursion", "monad", "class"]
-- ● Kimenet:
-- A karakterlancok, amelyek nem tartalmaznak szamokat:
--class
--float
--function
--Higher-order
--monad
-- ● Amennyiben nincsenek ilyen karakterláncok, a kimenet a következő:
-- “Nincsenek olyan karakterlancok, amelyek nem tartalmaznak szamot.”



szamjegyNelkul szovegek = do

    let szurtSzovegek = [szoveg | szoveg <- szovegek, not (any isDigit szoveg)]

    let rendezettSzovegek = sort szurtSzovegek

    if null rendezettSzovegek
        then putStrLn "Nincsenek olyan karakterlancok, amelyek nem tartalmaznak szamot."
        else do
            putStrLn "A karakterlancok, amelyek nem tartalmaznak szamokat:"
            mapM_ putStrLn rendezettSzovegek

fel3 = do
    --3. feladat tesztelése
    let szovegek = ["2023tuple", "function", "float", "higher-order", "variable10",
                    "may13be", "0recursion", "monad", "class"]
    szamjegyNelkul szovegek

--4. feladat

--4. Írjunk egy Haskell programot, amely meghatározza, hogy az s karakterláncnak
--melyek a szomszédjai az lsS karakterláncokat tartalmazó listából, ahol egy
--karakterlánc szomszédjait az ábécé sorrend szerinti kell érteni.
--Például:
-- ● Bemenet:
--s = feri
--lsS = Mari Zsuzsa szidi Lori kata feri teri Dani zsolti
-- ● Kimenet: feri baloldali szomszedja Zsuzsa, jobboldali szomszedja pedig kata

szomszedok :: String -> [String] -> IO ()
szomszedok s lsS = do

    let rendezettLista = sort lsS

    let baloldaliSzomszed = [x | x <- rendezettLista, x < s]
    let jobboldaliSzomszed = [x | x <- rendezettLista, x > s]

    let jobb = if null jobboldaliSzomszed then "nincs" else last jobboldaliSzomszed
    let bal = if null baloldaliSzomszed then "nincs" else head baloldaliSzomszed

    putStrLn $ "Rendezett lista: " ++ show rendezettLista
    putStrLn $ s ++ " baloldali szomszedja " ++ bal ++ ", jobboldali szomszedja pedig " ++ jobb

fel4 = do
    --4. feladat tesztelése
    let s = "zsolti"
    let lsS = ["Mari", "Zsuzsa", "szidi", "Lori", "kata", "feri", "teri", "Dani", "zsolti"]
    szomszedok s lsS

--5. feladat

--5. Egy [(String, Int, Int)] típusú lista eleme egy telefon márkanevet, egy eladási
--értéket, és egy árat tartalmaz. Írjunk egy Haskell programot, amely
--meghatározza azokat a telefonokat, amelyekből a legtöbbet adtak el, illetve
--mennyi volt ez az érték. Az eredmény márkaneveket rendezve egymás alá
--írjuk, amelyek elé írjuk ki egy kisérő szöveggel együtt a maximális eladási
--értéket.
--Például:
-- ● Bemenet: [("iphoneS1", 20, 2500), ("huaweiS1", 30, 1700), ("huaweiS2", 25,
--3100), ("samsungA1", 30, 2000), ("nokia", 10, 1900), ("iphoneS2", 10, 2200),
--("samsungA2", 15, 1650), ("iphone3", 30, 1800)]
-- ● Kimenet: A maximalis eladasi ertek 30. A telefonok, amelyeknek ennyi az
-- eladasi erteke a kovetkezok:
-- - iphone3
-- - huaweiS1
-- - samsungA1

legtobbEladottTelefonok telefonok = do

    let maxEladas = maximum [eladas | (_, eladas, _) <- telefonok]

    let csucsTelefonok = [marka | (marka, eladas, _) <- telefonok, eladas == maxEladas]

    let rendezettTelefonok = sort csucsTelefonok

    putStrLn $ "A maximalis eladasi ertek " ++ show maxEladas ++ ". A telefonok, amelyeknek ennyi az eladasi erteke a kovetkezok:"
    mapM_ (\telefon -> putStrLn $ "- " ++ telefon) rendezettTelefonok

fel5 = do
    --5. feladat tesztelése
    let telefonok = [("iphoneS1", 20, 2500), ("huaweiS1", 30, 1700), ("huaweiS2", 25, 3100),
                     ("samsungA1", 30, 2000), ("nokia", 10, 1900), ("iphoneS2", 10, 2200),
                     ("samsungA2", 15, 1650), ("iphone3", 30, 1800)]
    legtobbEladottTelefonok telefonok

--6. feladat

--6. Írj egy Haskell függvényt, melynek egy lista a bemenete, és megadja azokat a
--számokat, amelyek előfordulási száma páratlan. Az eredményt írasd ki a
--példában szereplő formában, előfordulási érték szerint rendezve.
--Például:
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

paratlanElofordulas szamok = do

    let csoportok = group $ sort szamok

    let gyakorisagok = [(length csoport, head csoport) | csoport <- csoportok]

    let paratlanok = [(db, ertek) | (db,ertek) <- gyakorisagok, odd db]

    let rendezett = sort paratlanok

    if null rendezett
        then putStrLn "Nincs paratlan elofordulasi ertekkel rendelkezo szam."
        else mapM_ (\(db, ertek) -> putStrLn $ "Elofordulas: " ++ show db ++ " -> Ertek: " ++ show ertek) rendezett
fel6 = do
    --6. feladat tesztelése
    let szamok = [1, 1, 2, 3, 4, 2, 6, 2, 4, 4, 2, 6, 7, 6, 6, 2]
    paratlanElofordulas szamok



--7.Egy [(String, Double, Int)] típusú lista elemei egy film címét, az értékelését (pl. 8.5) és a kiadás évét tartalmazzák. 
--Írj egy Haskell függvényt, amely meghatározza azokat a filmeket, amelyeket egy adott ev-nél később adtak ki, 
--ÉS az értékelésük szigorúan nagyobb, mint 8.0. A kapott filmcímeket ábécé sorrendbe rendezve, 
--kötőjellel listázva írd ki a képernyőre.

--Bemenet: 2010 [("Eredet", 8.8, 2010), ("Interstellar", 8.6, 2014), ("Dune", 8.0, 2021), ("Joker", 8.4, 2019), ("Avatar", 7.8, 2009)]

--Kimenet:

--Kivétel: Ha nincs ilyen film, a kimenet: "Nincs a felteteleknek megfelelo film."


filmek_rendez filmek n = do

    let szurFilmek = [cim | (cim, ertekeles, ev) <- filmek, ev > n && ertekeles > 8.0]

    let rendezFilmek = sort szurFilmek

    if null rendezFilmek
         then putStrLn "Nincs ilyen film."
            else do
                putStrLn "A felteteleknek megfelelo filmek a kovetkezok:"
                mapM_ (\film -> putStrLn $ "- " ++ film) rendezFilmek

fel7 = do
    --7. feladat tesztelése
    let filmek = [("Eredet", 8.8, 2010), ("Interstellar", 8.6, 2014), ("Dune", 8.0, 2021), ("Joker", 8.4, 2019), ("Avatar", 7.8, 2009)]
    filmek_rendez filmek 2010


palindromStringek ls = do

    let palindromok = [s | s <- ls, s == reverse s]

    if null palindromok
         then putStrLn "Nincs palindrom szo a listaban."
         else do
             putStrLn "A palindrom szavak a listaban:"
             mapM_ putStrLn palindromok

fel8 = do
    --8. feladat tesztelése
    let szavak = ["level", "hello", "world", "radar", "haskell", "madam"]
    palindromStringek szavak


gyakorisagSzamok szamok = do

    let csoportok = group $ sort szamok

    let gyakorisag = [head csoport | csoport <- csoportok, length csoport == 2]

    let rendezCsok = reverse $ sort gyakorisag

    if null gyakorisag
        then putStrLn "Nincs olyan szam, amely pontosan kétszer fordul elő."
        else do
            putStrLn "Azok a szamok, amelyek pontosan kétszer fordulnak elo: "
            mapM_ (\ertek -> putStrLn $ "Ertek: " ++ show ertek) rendezCsok


fel9 = do
    --9. feladat tesztelése
    let szamok = [1, 2, 3, 4, 2, 5, 6, 1, 7, 8]
    gyakorisagSzamok szamok



raktarKeszlet termekek = do

    let maxErtek = maximum [db * ar | (_, db, ar) <- termekek]

    let topTermek = [nev | (nev, db, ar) <- termekek, db * ar == maxErtek]

    if null topTermek
        then putStrLn "Nincs termek a raktarban."
        else do
            putStrLn $ "A legnagyobb ertek: " ++ show maxErtek ++ ". A termekek, amelyeknek ez az ertek: "
            mapM_ (\termek -> putStrLn $ "- " ++ termek) topTermek


fel10 = do
    --10. feladat tesztelése
    let termekek = [("Laptop", 5, 300000), ("Eger", 50, 10000), ("Monitor", 11, 150000), ("Billentyuzet", 20, 25000)]

    raktarKeszlet termekek


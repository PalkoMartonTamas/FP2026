-- Parcialis feladatok

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

szomszedok s lsS = do

    let rendezettLista = sort lsS

    let baloldaliSzomszed = [x | x <- rendezettLista, x < s]
    let jobboldaliSzomszed = [x | x <- rendezettLista, x > s]

    let jobb = if null jobboldaliSzomszed then "nincs" else last jobboldaliSzomszed
    let ball = if null baloldaliSzomszed then "nincs" else head baloldaliSzomszed

    putStrLn $ "Rendezett lista: " ++ show rendezettLista
    putStrLn $ s ++ " baloldali szomszedja " ++ ball ++ ", jobboldali szomszedja pedig " ++ jobb

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
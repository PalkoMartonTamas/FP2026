
import Data.List

{-1.	Írjunk Haskell programot, amely egy sorszám értékeket tartalmazó lista alapján meghatározza a megfelelő sorszámú Fibonacci számok listáját. A képernyőre külön sorokba írjuk ki a sorszámot, egy kettőspontot, majd a megfelelő Fibonacci számot.
Például a következő bemenetre: [10, 25, 1000, 0, 15, 5000]  
az eredmény: 
	10: 55
	25: 75025
	1000: 43466 … 28875
	0: 0
15: 610
5000: 38789 … 63125-}

fibonacciLista szamok = mapM_ fromazottKiiratas szamok
    where
        fromazottKiiratas n = putStrLn (show n ++ ": " ++ show (fibo n))

        fibo n = fiboAux (0, 1) n

        fiboAux (a, b) 0 = a
        fiboAux (a, b) k = fiboAux (b , a + b) (k - 1)

fel1 = do
    fibonacciLista [10, 25, 1000, 0, 15, 5000]


{- 2.	Egy [(String, Int)] típusú lista eleme egy városnevet és a megfelelő népességértéket tárolja. Írjunk egy Haskell függvényt, amely meghatározza, azokat a városokat, amelyek népességértéke egy adott n értéknél nagyobb. A kapott városneveket ábécé sorrendbe rendezve külön sorba írjuk ki a képernyőre.
Például a következő bemenetekre: n = 150000 [("sepsiszentgyorgy",54000),("kolozsvár",330000),("marosvasarhely",130000),("temesvar",310000),("arad",160000),("gyergyoszentmiklos",18000),("nagyvarad",196000)]
az eredmény:
arad
kolozsvár
nagyvarad
temesvar
-}

valogatVarosok varosok n = do

    let keresVarosok = [nev | (nev, nepesseg) <- varosok, nepesseg > n]

    let sortVarosNevek = sort keresVarosok

    if null sortVarosNevek
        then putStrLn $ "Nincsen " ++ show n ++ " nagyobb nepessegel rendelkezo varos!"
        else do
            mapM_ (\varos -> putStrLn $ "- " ++ varos) sortVarosNevek

fel2 = do

    let varosok = [("sepsiszentgyorgy",54000),("kolozsvár",330000),("marosvasarhely",130000),("temesvar",310000),("arad",160000),("gyergyoszentmiklos",18000),("nagyvarad",196000)]

    let n = 550000

    valogatVarosok varosok n


{-3.	Írjunk egy Haskell függvényt, amely meghatározza egy bemeneti egész számokat tartalmazó lista azon elemeit, amelyek nem tartalmazzák a 0 számjegyet. Az eredményszámokat szóközökkel elválasztva írjuk ki a képernyőre.
Például a következő bemenetre: [17603, 4005, 3223, 816252, 70, 23561, 9018007, 807, 61, 300]
az eredmény: 3223 816252 23561 61
-}

szamLista szamok = do

    let szurtSzamok = [x | x <- szamok, notElem '0' (show x)]

    putStrLn $ unwords (map show szurtSzamok)

fel3 = do

    szamLista [17603, 4005, 3223, 816252, 70, 23561, 9018007, 807, 61, 300]
{-2.	Olvassunk be a billentyűzetről két számot és határozzuk meg a két szám közötti Hamming számokat, majd írjuk ki őket külön sorba.
Példa:  
830, 840 
15943230
16000000
16200000
16384000
16402500
16588800
16777216
16796160
16875000
17006112
17280000
-}

merge (x : xs) (y : ys)
    | x < y = x : merge xs (y : ys)
    | x > y = y : merge (x : xs) ys
    | otherwise = x : merge xs ys

hammingSzamok = 1 : merge (map (* 2) hammingSzamok) (merge (map (* 3) hammingSzamok) (map (* 5) hammingSzamok))

fel2 = do
    putStrLn "Adj meg ket szamot vesszovel elvalasztva: "
    input <- getLine

    let tisztitott = map (\c -> if c == ',' then ' ' else c) input
        szamok = map read (words tisztitott) :: [Int]
        kezdo = szamok !! 0
        vegzo = szamok !! 1

    let dbszam = vegzo - kezdo + 1
        eredmeny = take dbszam (drop (kezdo - 1) hammingSzamok)

    mapM_ print eredmeny

{-3.	Egy [(String, Int)] típusú lista elemei városneveket és a megfelelő népességértékeket tárolják. Írjunk Haskell függvényt, amely meghatározza egy adott n értéknél nagyobb népességgel rendelkező városok átlagnépességét.
Példa:
Bemenet:
n = 150000 [("sepsiszentgyorgy",54000),("kolozsvár",330000),("marosvasarhely",130000),("temesvar",310000),("arad",160000),("gyergyoszentmiklos",18000),("nagyvarad",196000)]
Az eredmény:
249000.0
-}

varosAtlagLakossag varosok n = do

    let varosokNepessege = [nepesseg | (_, nepesseg) <- varosok, nepesseg > n]

    let atlag ls = sum ls / fromIntegral (length ls)

    let atlagSzamol = atlag varosokNepessege

    if null varosokNepessege 
        then putStrLn "Nincs ilyen varos!"
        else do
            putStrLn $ "Az eredmeny: "
            print atlagSzamol

fel3 = do
    let varosok = [("sepsiszentgyorgy",54000),("kolozsvár",330000),("marosvasarhely",130000),("temesvar",310000),("arad",160000),("gyergyoszentmiklos",18000),("nagyvarad",196000)]
    let n = 150000

    varosAtlagLakossag varosok n
import Data.List(sort)

{-1. Írjunk egy Haskell programot, amely meghatározza, hogy az s karakterláncnak melyek a szomszédjai az
lsS karakterláncokat tartalmazó listából, ahol egy karakterlánc szomszédjait az ábécé sorrend szerinti
kell érteni.
Például a következő bemenetre
s = feri
lsS = Mari Zsuzsa szidi Lori kata feri teri Dani zsolti
az eredmény: Zsuzsa kata-}


szomszedok :: String -> [String] -> IO ()
szomszedok s ls = do

    let rendezettLs = sort ls

    let jobbSzomszed = [x | x <- rendezettLs, x < s]
    let balSzomszed = [x | x <- rendezettLs, x > s]

    let jobb = if null jobbSzomszed then "nincs" else last jobbSzomszed
    let bal = if null balSzomszed then "nincs" else head balSzomszed

    putStrLn $ "Rendezett lista: " ++ show rendezettLs
    putStrLn $ show s ++ " baloldali szomszedja: " ++ bal ++ ", jobboldali szomszedja pedig: " ++ jobb


fel1 = do
    --4. feladat tesztelése
    let s = "zsolti"
    let lsS = ["Mari", "Zsuzsa", "szidi", "Lori", "kata", "feri", "teri", "Dani", "zsolti"]
    szomszedok s lsS

{-3. Egy [(String, Int, Int)] típusú lista eleme egy telefon márkanevet, egy eladási értéket, és egy
árat tartalmaz. Írjunk egy Haskell programot, amely meghatározza azokat a telefonokat, amelyekből a
legtöbbet adtak el, illetve mennyi volt ez az érték. Az eredmény márkaneveket rendezve egymás alá
írjuk, amelyek elé írjuk ki egy kisérőszöveggel együtt a maximális eladási értéket.
Például a következő bemenetre
[("iphoneS1",20,2500), ("huaweiS1",30,1700), ("huaweiS2",25,3100),
("samsungA1",30,2000), ("nokia",10,1900), ("iphoneS2",10,2200),
("samsungA2",15,1650), ("iphone3",30,1800)]
az eredmény:
maximális eladási érték: 30
iphone3
huaweiS1
samsungA1-}


telefonKezel telefonok = do

    let maxErtek = maximum [ertek | (_,ertek,_) <- telefonok]

    let keresMaxTelo = [nev | (nev, ertek, ar) <- telefonok, ertek == maxErtek]

    let rendezTelo = sort keresMaxTelo

    if null keresMaxTelo
        then putStrLn "Ures lista"
        else do
            putStrLn $ "Az eredmeny: "
            putStrLn $ "A max eladasi ertek: " ++ show maxErtek
            mapM_ (\nev -> putStrLn $ nev) rendezTelo

fel3 = do

    let telefonok = [("iphoneS1",20,2500), ("huaweiS1",30,1700), ("huaweiS2",25,3100),
                        ("samsungA1",30,2000), ("nokia",10,1900), ("iphoneS2",10,2200),
                        ("samsungA2",15,1650), ("iphone3",30,1800)]

    telefonKezel telefonok


{-2. Egy listában számok vannak, írjuk ki azokat a számokat a képernyőre egymás alá, amelyek 0-s
számjegyben végződnek.
Például a következő bemenetre
[120, 456, 3213, 67, -100, -56, -20, 112, 354]
az eredmény: ls =3
120
-100
-20
-}

nullaVege :: [Int] -> IO ()
nullaVege szamok = do

    let szurtSzamok = [x | x <- szamok, mod x 10 == 0]

    putStrLn $ "Az eredmeny: " ++ "ls: " ++ show (length szurtSzamok)
    mapM_ print szurtSzamok 

fel2 = do
    let szamok = [120, 456, 3213, 67, -100, -56, -20, 112, 354]

    nullaVege szamok
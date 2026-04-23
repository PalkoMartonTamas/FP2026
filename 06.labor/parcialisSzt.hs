import Data.Fixed (HasResolution(resolution))
import System.Win32 (LOCALESIGNATURE(lsCsbDefault))
{-1. Egy listában egy diák jegyei szerepelnek. Írjunk egy Haskell programot, amely meghatározza azokat a
jegyeket, amelyek kisebbek, nagyobbak, egy adott jegynél illetve hány egyenlő vele. Az eredményt írjuk ki
formázva a példában megadottak szerint:
Például, ha az adott jegy 6.5 és a jegyek lista tartalma a következő:
[9.5, 10, 6.5, 4.5, 5.5, 7, 7.25, 7.8, 9, 3.5, 6.5, 9.75, 3.5 ], akkor az eredmény a következő:
nagyobb jegyek: 9.5, 10, 7, 7.25, 7.8, 9, 9.75, számuk 7
6.5-el egyenlő jegyek száma: 2
kisebb jegyek: 4.5, 5.5, 3.5, 3.5, számuk: 4
ha az adott jegy 2 és a jegyek lista az előző, akkor az eredmény a kövekező:
nagyobb jegyek: 9.5, 10, 6.5, 4.5, 5.5, 7, 7.25, 7.8, 9, 3.5, 6.5, 9.75, 3.5, számuk 13
2-vel egyenlő jegyek száma: 0
kisebb jegyek: nincsenek-}


diakokJegyei jegyek n = do

    let nagyobbJegyek = [jegy | jegy <- jegyek , jegy > n]
    let nagyobbJegyekSzama = length nagyobbJegyek

    let egyenloJegyek = [jegy | jegy <- jegyek, jegy == n]
    let egyenloJegyekSzama = length egyenloJegyek

    let kisebbJegyek = [jegy | jegy <- jegyek, jegy < n]
    let kisebbJegyekSzama = length kisebbJegyek

    putStrLn $ "nagyobb jegyek: " ++ unwords (map show nagyobbJegyek) ++ ", szamuk: " ++ show nagyobbJegyekSzama
    putStrLn $ show n ++ "-el egyenlo jegyek szama: " ++ show egyenloJegyekSzama
    putStrLn $ "kisebb jegyek: " ++ unwords (map show kisebbJegyek) ++ ", szamuk: " ++ show kisebbJegyekSzama

fel1 = do

    let jegyek = [9.5, 10, 6.5, 4.5, 5.5, 7, 7.25, 7.8, 9, 3.5, 6.5, 9.75, 3.5 ]
    let n = 6.5

    diakokJegyei jegyek n


{-2. A szemelyek lista három elemű tuple típusú értékeket tartalmaz, ahol egy személyről a következő adatok
vannak eltárolva: emailcím, ervényességi idő, bankkártyaszám. Írjunk egy Haskell programot, amely
a. meghatározza, hogy kik azok a szemelyek, és mi a bankkártyaszámuk, akiknek leghamarabb lejár a
bankkártyájuk
b. minden személy esetében felbontja az email címet felhasználónév, mail szerver és domain
részekre, majd az eredményt kiírja formázva.
Például, ha a szemelyek lista tartalma a következő:
[("rosalesanthony@example.net", "03/31", "213130957725524"),
("robin18@example.net", "02/29", "570620146482"),
("bsullivan@example.org", "03/27", "4215057708441701869"),
("jameshughes@example.org", "09/27", "4782851642138996"),
("douglasjordan@example.net", "03/27", "5289954454350249"),
("jwells@example.net", "06/31", "342926219737676"),
("spotter@example.com", "01/27", "4917299108623093")]
akkor az eredmények:
a.
bsullivan@example.org, 03/27, 4215057708441701869
jameshughes@example.org, 09/27, 4782851642138996
douglasjordan@example.net, 03/27, 5289954454350249
spotter@example.com, 01/27, 4917299108623093
b.
felhasználónév mail-szerver domain
rosalesanthony example net
robin18 example net
bsullivan example org
jameshughes example org
douglasjordan example net
jwells example net
spotter example com-}

getYear datum = drop 3 datum

csere c
    | c == '@' || c == '.' = ' '
    | otherwise = c


fel2_A lista = do

    putStrLn "a."

    let minEv = minimum [getYear datum | (email, datum, bankszam) <- lista]

    let leghamarabbLejar = [(email, datum, bankszam) | (email, datum, bankszam) <- lista, getYear datum == minEv]

    mapM_ (\(e, d, b) -> putStrLn (e ++ ", " ++ d ++ ", " ++ b)) leghamarabbLejar


fel2_B lista = do

    putStrLn "b."
    putStrLn "felhasznalonev    email szerver   domain"

    let emailek = [email | (email, _, _) <- lista]

    let bontottEmail = map (\email -> words (map csere email)) emailek

    mapM_ (\szavak -> putStrLn (unwords szavak)) bontottEmail


fel2_B_2 lsE = mapM_ myPrint resL

    where 
        myPrint (t1, t2, t3) = putStrLn (t1 ++ " " ++ t2 ++ " " ++ t3)
        resL = map fgM lsE
        fgM (t1, t2, t3) = (userN, szerverN, domainN)
            where
                userN = takeWhile (/= '@') t1
                rest = dropWhile (/= '@') t1
                szerverN = tail $ takeWhile (/= '.') rest
                domainN = tail $ dropWhile (/= '.') rest  

fel2 = do
    
    let szemelyek = [("rosalesanthony@example.net", "03/31", "213130957725524"),
                     ("robin18@example.net", "02/29", "570620146482"),
                     ("bsullivan@example.org", "03/27", "4215057708441701869"),
                     ("jameshughes@example.org", "09/27", "4782851642138996"),
                     ("douglasjordan@example.net", "03/27", "5289954454350249"),
                     ("jwells@example.net", "06/31", "342926219737676"),
                     ("spotter@example.com", "01/27", "4917299108623093")]

    fel2_A szemelyek
    --fel2_B szemelyek
    putStrLn "b."
    putStrLn "felhasznalonev    email   domain"
    fel2_B_2 szemelyek
    
          
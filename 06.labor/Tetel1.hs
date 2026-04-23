{-1.	Olvassunk be karakterláncokat a billentyűzetről és határozzuk meg, hogy melyik karakterláncokban van a legtöbb magánhangzó. Az eredmény karakterláncokat szóközökkel elválasztva írjuk ki a képernyőre.
Példa: 
Bemenet: marika, ANNABELLA, szidonia, lorand, katalin, feri, terezia, Daniel, zsolti
Az eredmény: ANNABELLA szidonia terezia
-}

csere c
    | c == ',' = ' '
    | otherwise = c

isMaganH c = elem c "aeiouAEIOU"

szamolMaganH szo = length [c | c <- szo, isMaganH c]

legtobbMaganH = do
    putStrLn "Adj meg neveket vesszovel elvalasztva: "
    input <- getLine

    let szavak = words(map csere input)

    let szavakEsSzamok = [(szo, szamolMaganH szo) | szo <- szavak]

    let maxDb = maximum [db | (_, db) <- szavakEsSzamok]

    let eredmenySzavak = [szo | (szo, db) <- szavakEsSzamok, db == maxDb]

    putStrLn $ "Az eredmeny: " ++ unwords eredmenySzavak


fel1 = do

    legtobbMaganH
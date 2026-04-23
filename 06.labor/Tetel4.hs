
valtakozSzo n = take n ls
    where 
        ls = ["IGEN", "NEM", "TARTOZKODO"] ++ ls


nincsNullaBenne szamok = do

    let szurtSzamok = [x | x <- szamok, notElem '0' (show x)]

    mapM_ print szurtSzamok

fel1 = do

    let szamok = [17063,32,80003,32450,224,2]

    nincsNullaBenne szamok


vernyomasSzabalyzo lista = do

    let szurtLista = [(x1, x2) | (x1, x2) <- lista, x1 > 140 || x2 > 159]

    if null szurtLista
        then putStrLn "Nincs magas vernyomas"
        else do
            putStrLn "Az eredmeny: "
            mapM_ (\(t1, t2) -> putStrLn $ show t1 ++ ", " ++ show t2)szurtLista

fel3 = do

    let lista = [(110, 60), (141, 80), (110, 75), (145,179),(111,85),(100,85),(105,65),(101,95),(100,70),(131,170)]

    vernyomasSzabalyzo lista
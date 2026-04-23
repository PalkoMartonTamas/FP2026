import System.Win32 (LOCALESIGNATURE(lsCsbDefault))
osszeg :: Int -> Int -> Int
osszeg a b = a + b

kulonbseg a b = a - b

szorzat a b = a * b

hanyados :: (Fractional a) => a -> a -> a
hanyados a b = a / b

hanyados2 :: (Integral a) => a -> a -> a
hanyados2 a b = div a b

hanyados3 :: (Integral a) => a -> a -> a
hanyados3 a b = a `div` b

maradek :: Integral a => a -> a -> a
maradek a b = mod a b

maradek2 :: Integral a => a -> a -> a
maradek2 a b = a `mod` b

elsoF :: Fractional a => a -> a -> a
elsoF a b = (-b) / a

abszolut a
    | a < 0 = -a
    | otherwise = a

abszolut2 a = if a < 0 then -a else a

-- szam elojele
elojel n = if n < 0 then "negativ" else if n > 0 then "positive" else "nulla"

elojel2 n
    | n < 0 = "negative"
    | n > 0 = "positive"
    | otherwise = "nulla"

max_ a b = if a > b then a else b

max1 a b
    | a > b = a
    | otherwise = b

min_ a b
    | a < b = a
    | otherwise = b

--masodfoku egyenlet gyokei
masodf a b c = if delta < 0 then error "komplex szamok" else (gy1,gy2)
    where
        delta = b**2 - 4*a*c
        gy1 = (-b + sqrt delta) / (2 * a)
        gy2 = (-b - sqrt delta) / (2 * a)

masodf_2 a b c
    | delta < 0 = error "komplex szamok"
    | delta == 0 = [gy1]
    | otherwise = [gy1, gy2]
    where
        delta = b**2 - 4*a*c
        gy1 = (-b + sqrt delta) / (2 * a)
        gy2 = (-b - sqrt delta) / (2 * a)


-- hogy két elempár értékei "majdnem" megegyeznek-e: akkor térít vissza True értéket a függvény, 
-- ha a két pár ugyanazokat az értékeket tartalmazza függetlenül az elemek sorrendjétől.

elempar ep1 ep2 = (a == d && b == c) || (a == c && b == d)
    where
        (a,b) = ep1
        (c,d) = ep2

elempar2 (a,b) (c,d) = (a == c && b == d) || (a == d && b == c)


fakt1 0 = 1
fakt1 n = n * fakt1 (n - 1)

fakt2 n
    | n < 0 = error "negativ szam"
    | n == 0 = 1
    | otherwise = n * fakt2 (n-1)

-- pl. meghivast: fakt3 5 1
fakt3 n res
    | n < 0 = error "negativ szam"
    | n == 0 = res
    | otherwise = fakt3 (n-1) (res*n)


hatvany :: (Ord a, Floating a) => a -> a -> a
hatvany x n
    | n < 0 = error "negativ kitevo"
    | otherwise = x ** n

hatvany2 :: (Num a, Integral b) => a -> b -> a
hatvany2 x n
    | n < 0 = error "negativ kitevo"
    | otherwise = x ^ n

hatvany3 x n
    | n < 0 = error "negativ kitevo"
    | n == 0 = 1
    | otherwise = x * hatvany3 x (n - 1)



main :: IO ()
main = do
    putStrLn "Masodfoku egyenlet"
    print (masodf 1 2 1)
    putStrLn ("Masodfoku egyenlet 2 : " ++ show (masodf_2 1 2 1))
    putStrLn "Elemparok: "
    print (elempar2 (6,7) (7,6))
    putStrLn "Faktorialis : "
    print (fakt2 5)
    print (fakt2 10)
    putStrLn ("Hatvany: " ++ show (hatvany 2 4))
    --II.
    putStrLn ("Negyzet gyokok: " ++ show (negyzetgyok 10))
    putStrLn ("Negyzet szamok: " ++ show (negyzetszam 10))
    putStrLn ("Kob szamok: " ++ show (kobszam 10))

--II.

negyzetgyok n = [sqrt i | i <- [1..n]]


negyzetszam n = [i ^ 2 | i <- [0..n]]


kobszam n = [i ^ 3 | i <- [0..n]]


nemNegyzet n = [ i | i <- [1..n], i /= (sqrt i ** 2)]


hatvanyX x n = [x ^ i | i <-[0..n]]


parosOsztok x = [i | i <- [1..x], mod x i == 0, mod i 2 == 0]


osztok x = [i | i <- [1..x], mod x i == 0]
primszam x = osztok x == [1,x]
primszamN n = [i | i <-[1..n], primszam i]

primszamN2 n = [i | i <- [1..n], primszamL i]
    where
        primszamL si = osztokL si == [1,si]
        osztokL si2 = [i | i <- [1..si2], mod si2 i == 0]


osszetett n = [i | i <- [1..n], primszam i == False]

osszetett2 n = [i | i <- [1..n], not (primszam i)]


paratlanOsszetett n = [i | i <- [1..n], not (primszam i), mod i 2 /= 0]

paratlanOsszetett2 n = [i | i <- [1..n], not (primszam i), odd i]


-- a**2 + b**2 = c**2
pitagorasz n = [(a,b,c) | c <- [1..n], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]


-- a következő listát: $$[(\texttt{a},0), (\texttt{b},1),\ldots, (\texttt{z}, 25)]$$,
betuSzam = zip ['a'..'z'] [0..25]

-- a következő listát: $$[(0, 5), (1, 4), (2, 3), (3, 2), (4, 1), (5, 0)]$$, majd általánosítsuk a feladatot.
szamok = zip [0..5] [5,4..0]

szamokN n = zip [0..n] [n,n-1..0]

szamok3 n = [(i,n-i) | i <- [0..n]]

-- azt a listát, ami felváltva tartalmaz True és False értékeket.

tf n = [mod i 2 == 0 | i <-[0..n]]

tf2 n = take n ls
    where 
        ls = [True,False] ++ ls

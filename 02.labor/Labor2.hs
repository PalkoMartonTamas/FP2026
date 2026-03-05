import Control.Monad.Trans.Cont (reset)
import Graphics.Win32 (eWX_LOGOFF)
-- 2. labor

--I. Könyvtárfüggvények használata nélkül, definiáljuk azt a függvényt, amely meghatározza:

-- egy szám számjegyeinek szorzatát (2 módszerrel),
szjSzorzat 0 = 1
szjSzorzat n = (mod n 10) * szjSzorzat (div n 10)


szjSzorzat2 n
    | n < 0 = szjSzorzat2 (abs n)
    | div n 10 == 0 = mod n 10
    | otherwise = mod n 10 * szjSzorzat2 (div n 10)

ls1 = [234,24,324,2,0]

szjSzorzatList ls = map szjSzorzat2 ls

szjSzorzatList2 ls = map (\x -> (x, szjSzorzat2 x)) ls


-- egy szám számjegyeinek összegét (2 módszerrel),
szjOsszeg n
    | n < 0 = szjSzorzat2 (abs n)
    | div n 10 == 0 = mod n 10 -- n < 10 = n
    | otherwise = mod n 10 + szjOsszeg (div n 10)

szjOsszeg2 n res
    | n < 0 = szjOsszeg2 (abs n) res
    | n < 10 = res + n
    | otherwise = szjOsszeg2 (div n 10) (res + mod n 10)

szjOsszegList ls = map szjOsszeg ls

szjOsszegList2 ls = map (\x -> (x,szjOsszeg x)) ls



-- egy szám számjegyeinek számát (2 módszerrel),
szjSzama n res
    | n < 0 = szjSzama (abs n) res
    | n < 10 = res + 1
    | otherwise = szjSzama (div n 10) (res + 1)

szjSzama2 n
  | n < 0 = szjSzama2 (abs n)
  | n < 10 = 1
  | otherwise = 1 + szjSzama2 (div n 10)

szjSzamList ls = map szjSzama2 ls


-- egy szám azon számjegyeinek összegét, mely paraméterként van megadva, pl. legyen a függvény neve fugv4, ekkor a következő meghívásra, a következő eredményt kell kapjuk:

 -- ```haskell
--  > fugv4 577723707 7
--  35
 -- ```
fugv4 n szj
  | szj > 9 = error "nem szj"
  | n < 10 = if n == szj then szj else 0
  | otherwise =
      if mod n 10 == szj
      then szj + fugv4 (div n 10) szj
      else fugv4 (div n 10) szj

szjSzamOsszeg2 n szj elof
  | szj > 9 = error "nem szj"
  | n < 10 = if n == szj then (elof + 1) * szj else elof * szj
  | otherwise = if mod n 10 == szj then szjSzamOsszeg2 (div n 10) szj (elof + 1) else szjSzamOsszeg2 (div n 10) szj elof

ls2 = [(577723707,7),(423,3),(0,1),(12,2)]
szjSzamOsszegLs ls = map (uncurry fugv4) ls

szjOsszegLs2 ls = map (\(x, szj) -> fugv4 x szj) ls

-- egy szám páros számjegyeinek számát,
parosSzamSzj n
  | n < 0 = parosSzamSzj (abs n)
  | n < 10 = if even n then 1 else 0
  | otherwise =
      if even (mod n 10)
        then 1 + parosSzamSzj (div n 10)
        else parosSzamSzj (div n 10)

parosSzamSzj2 n res
  | n < 0 = parosSzamSzj2 (abs n) res
  | n < 10 = if even n then res + 1 else res
  | otherwise =
    if even (mod n 10)
      then parosSzamSzj2 (div n 10) (res + 1)
      else parosSzamSzj2 (div n 10) res

parosSzamSzjLs ls = map parosSzamSzj ls

-- egy szám legnagyobb számjegyét,
lgSzj n ln
  | n < 0 = lgSzj (abs n) ln
  | n < 10 = max n ln
  | otherwise =
    if mod n 10 > ln
      then lgSzj (div n 10) (mod n 10)
      else lgSzj (div n 10) ln

lgSzjLs ls = map (\x -> lgSzj x 0) ls

-- egy szám $b$ számrendszerbeli alakjában a $d$-vel egyenlő számjegyek számát (például a $b = 10$-es számrendszerben a $d = 2$-es számjegyek száma),
  --Példák függvényhívásokra:

--  ```haskell
--  fugv 7673573 10 7 -> 3
--  fugv 1024 2 1 -> 1
--  fugv 1023 2 1 -> 10
--  fugv 345281 16 4 -> 2
--  ```
bSzamDSzj n b d
  | n < 0 = bSzamDSzj (abs n) b d
  | n < b = if n == d then 1 else 0
  | otherwise = 
    if mod n b == d then 1 + bSzamDSzj (div n b) b d
    else bSzamDSzj (div n b) b d

ls3 = [(7673573, 10, 7),(1024, 2, 1),(1023, 2, 1),(345281, 16, 4)]

bSzamDSzjLs ls = map (\(n, b, d) -> bSzamDSzj n b d) ls3

-- az 1000-ik Fibonacci számot.
fibo a b res n 
  | n == 0 = res
  | otherwise = fibo b res (res + b) (n - 1)

fiboN :: (Eq t1, Num t1, Num t2) => t1 -> t2
fiboN n = fibo 0 1 0 n

fiboN2 n = fiboSg 0 1 0 n
  where
    fiboSg _ _ res 0 = res
    fiboSg a b res n = fiboSg b res (res + b) (n -1)

fiboSzamok n = map fiboN [0..n]


--II. Alkalmazzuk a map függvényt a I.-nél megírt függvényekre.

-- **Megoldott feladatok:**

-- Határozzuk meg egy szám számjegyeinek összegét:
--  I. módszer:

--  ```haskell
--  szOsszeg :: Int -> Int
--  szOsszeg 0 = 0
--  szOsszeg x = ( x `mod` 10 ) + szOsszeg (x `div` 10)

--  > szOsszeg 123
--  ```

--  II. módszer:

--  ```haskell
--  szOsszeg1 :: Int -> Int -> Int
--  szOsszeg1 0 t = t
--  szOsszeg1 x t = szOsszeg1 (x `div` 10) ( t + x `mod` 10 )

--  > szOsszeg1 123 0
--  ```

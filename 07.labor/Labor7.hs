-- # 7. labor

--I. Írjunk egy-egy Haskell függvényt, amely

-- az n-nél kisebb négyzetszámokat kiírja egy szövegállományba,
-- az n-nél kisebb számok négyzetgyökét kiírja egy szövegállományba. A nr négyzetgyök meghatározásához használjuk a következőket, ahol az iterációt addig kell végezni, amíg $x_{n+1}$ nem egyenlő $x_n$-nel:

--  ```
--  x_0 = 1
--  x_{n+1} = (x_n + nr/x_n)/2
--  ```
-- az n-nél kisebb számok köbgyökét kiírja egy szövegállományba.A köbgyök meghatározásához használjuk a következőket, ahol az iterációt addig kell végezni, amíg $x_{n+1}$ nem egyenlő $x_n$-nel:

--  ```
--  x_0 = 1
--  x_{n+1} = (2·x_n + nr/(x_n·x_n))/3
--  ```

--II. Írjunk egy-egy Haskell függvényt, amely szövegállományban levő számokat olvas be egy listába, és kiírja formázva egy másik szövegállományba

-- a számok rendezett sorrendjét,
-- a számokkal együtt a számok 2-es számrendszerbeli alakját, illetve, hogy hány egyes szerepel a 2-es számrendszerbeli alakban,
-- a számokkal együtt a számok 2, 16, 256-os számrendszerbeli alakját,
-- a számokkal együtt a számok prímosztóit.

--III. Írjunk egy Haskell függvényt, amely amely kigenerálja egy állományba

-- az $a$ és $b$ közötti Hamming számokat, használjuk a takeWhile, dropWhile függvényeket ($a > 300$),
-- az 10000-nél kisebb prímszámokat, a prímszámokat Eratoszthenész szitájával határozzuk meg,
-- az 10000-nél kisebb szerencsés számokat ([Lucky number](https://en.wikipedia.org/wiki/Lucky_number)).

--IV. Írjunk egy-egy Haskell függvényt, amely

-- meghatározza, hogy két bináris állományban milyen pozíciókon található különböző bájt,
-- megvizsgálja, hogy egy adott bájtszekvencia benne van-e egy bináris állományban,
-- meghatározza egy adott állomány bájtméretét, ahol az állománynevet a billentyűzetről olvassuk be,
-- meghatározza bináris állományok méret szerinti rendezett sorrendjét, ahol az állományneveket a billentyűzetről olvassuk be,
-- másolatot készít bináris állományokról, ahol az állományok nevét a billentyűzetről olvassuk be,

--V. Írjunk egy Haskell programot, amely titkosítja karakterek (bájtok) egy adott listáját, majd vissza is fejti a rejtjelezett értéket:

-- a titkosításhoz egy titkos információt, egy kulcsot (karaktereket/bájtokat) kell megadni,
-- a titkosítás azt fogja jelenti, hogy a bemeneti bájtok és a kulcs bájtjai között alkalmazzuk az xor műveletet, úgy hogy a kulcs bájtjait körkörösen vesszük, ami azt jelenti, hogy ha elfogytak a kulcs bájtjai, akkor a kulcs első bájtjával folytatjuk az xor műveletet, egészen addig, amíg a bemenet bájtjain is végig nem mentünk,
-- a helyes működés miatt fontos, hogy ugyanazt a kulcsot használjuk mind a titkosításhoz, mind a visszafejtéshez,
-- a titkosított értéket hexadecimális string-ként írjuk ki,
-- a program során legyen választási lehetőség arra vonatkozóan, hogy a kulcs értékét:
  -- beolvassuk a billentyűzetről, mint hexadecimális string
  -- véletlenszerűen generáljuk, mint 0 és 255 közötti természetes számok.

--Például:

--```haskell
-- > bemenet = "sapientia marosvasarhelyi tudomanyegyetem"
-- > kulcs = "c 38 ff 66 71 22 38 4e 79 65"
-- > cryptStr bemenet kulcs
-- titkositott ertek: 7f 59 8f f 14 4c 4c 27 18 45 61 59 8d 9 2 54 59 3d 18 17 64 5d 93 1f 18 2 4c 3b 1d a 61 59 91 1f 14 45 41 2b d 0 61
-- ```

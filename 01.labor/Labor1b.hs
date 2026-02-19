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
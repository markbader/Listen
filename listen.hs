l1 = [1,2,4,6,1,0]
l2 = [1,1,1,2,2,3,3,5,6]
l3 = [1,2,3,4,5,6,7,8,10,14,17]
l4 = [4,7,21,7,9,2,7,5,5,99,98,100,4]

len :: Eq a => [a] -> Int
len x
 |x == [] = 0
 |otherwise = 1 + len(tail x)

laenge :: [a] -> Int
laenge [] = 0
laenge (x:xs) = 1 + laenge xs

maxi :: Ord a => [a] -> a
maxi [] = error "No elements!"
maxi [x] = x
maxi (x:xs)
 |x < maxi xs = maxi xs
 |otherwise = x

anzEl :: Ord a => [a] -> a -> Int
anzEl [] y = 0
anzEl (x:xs) y
 |x == y =1 + anzEl xs y
 |otherwise = anzEl xs y

maxi2 :: Ord a => [a] -> a
maxi2 [x] = x
maxi2 (x:y:xs)
 |x < y = maxi2 (y:xs)
 |otherwise = maxi2 (x:xs)

dupl :: Eq a => [a] -> [a]
dupl [] = []
dupl (x:xs) = x:x:dupl xs

kopf :: [a] -> a
kopf (x:xs) = x

rest :: [a] -> [a]
rest (x:xs) = xs

ist_enthalten :: Eq a => [a] -> a -> Bool
ist_enthalten [] y = False
ist_enthalten [x] y
 |x == y = True
 |otherwise = False
ist_enthalten (x:xs) y
 |x == y || ist_enthalten xs y = True
 |otherwise = False

nimm0 :: Eq a => [a] -> Int -> [a]
nimm0 [] 0 = []
nimm0 (x:xs) 0 = []
nimm0 (x:xs) y
 |y > laenge (x:xs) = nimm0 xs (y - 1)
 |otherwise = x:nimm0 xs (y -  1)

nimm :: Int -> [a] -> [a]
nimm 0 xs = []
nimm n [] = []
nimm n (x:xs) = x:nimm(n-1) xs

loesche :: Eq a => [a] -> a -> [a]
loesche [] y = []
loesche (x:xs) y
 |y == x = loesche xs y
 |otherwise = x:loesche xs y

loesche_eins :: Eq a => [a] -> a -> [a]
loesche_eins [] y = []
loesche_eins (x:xs) y
 |y == x = xs
 |otherwise = x:loesche_eins xs y

loesche_dups0 :: Ord a => [a] -> [a]
loesche_dups0 [] = []
loesche_dups0 (x:xs)
 |anzEl xs x > 0 = loesche_dups0 xs
 |otherwise = x:loesche_dups0 xs

--loesche_dups :: 
 
ist_geordnet :: Ord a => [a] -> Bool
ist_geordnet [] = True
ist_geordnet [x] = True
ist_geordnet (x:y:xs)
 |x > y || not (ist_geordnet xs) = False
 |otherwise = True
 
einfuegen :: Ord a => [a] -> a -> [a]
einfuegen [] y = [y]
einfuegen (x:xs) y
 |y < x = (y:x:xs)
 |otherwise = x:(einfuegen xs y)
 
zaehlen :: Eq a => [a] -> a -> Int
zaehlen [] y = 0

umkehren :: [a] -> [a]
umkehren [] = []
umkehren [x] = [x]
--umkehren (x:xs) = 

kette :: [a] -> [a] -> [a]
kette [] xs = xs
kette (x:xs) ys = x:kette xs ys

fibon :: Int -> Int
fibon 0 = 1
fibon 1 = 1
fibon n = fibon (n - 1) + fibon (n - 2)

einfuegesort :: Ord a => [a] -> [a]
einfuegesort [] = []
einfuegesort xs = einf xs [] --einf :: [a] -> [a] -> [a]
einf [] akku = akku
einf (x:xs) akku = einf xs (einfuegen akku x)
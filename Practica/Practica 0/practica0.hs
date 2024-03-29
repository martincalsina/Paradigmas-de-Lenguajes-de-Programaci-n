import Prelude

--Ejercicio 1

--es básicamente hacer :t nombreDeLaFuncion en el ghci y ver qué devuelve, probarla con unos valores y 
--explicar qué es lo que está haciendo

--Ejercicio 2

--a
valorAbsoluto :: Float -> Float
valorAbsoluto x | x < 0 = -x
                | x >= 0 = x

--b
bisiesto :: Int -> Bool
bisiesto x | x `mod` 4 == 0 && (x `mod` 100 /= 0 || x `mod` 400 == 0)= True
           | otherwise = False

--c
factorial :: Int -> Int
factorial 0 = 1
factorial x = x * factorial (x - 1)

--d
cantDivisoresPrimos :: Int -> Int
cantDivisoresPrimos n = sumarDivisoresPrimos n n

sumarDivisoresPrimos :: Int -> Int -> Int
sumarDivisoresPrimos _ 1 = 0
sumarDivisoresPrimos n x | (esDivisor n x) && (esPrimo x) = 1 + (sumarDivisoresPrimos n (x-1))
                        | otherwise = sumarDivisoresPrimos n (x-1)

esDivisor :: Int -> Int -> Bool
esDivisor n x = n `mod` x == 0

esPrimo :: Int -> Bool
esPrimo x = cantDivisores x x == 2

cantDivisores :: Int -> Int -> Int
cantDivisores n 1 = 1
cantDivisores n x | n `mod` x == 0 = 1 + (cantDivisores n (x-1))
                  | otherwise = cantDivisores n (x-1)


--Ejercicio 3

--data Maybe a = Nothing | Just a
--data Either a b = Left a | Right b

--a
inverso :: Float -> Maybe Float
inverso  x | x == 0 = Nothing
           | otherwise = Just (1 / x)

--b
aEntero :: Either Int Bool -> Int
aEntero (Left x) = x
aEntero (Right x) | x == True = 1
                  | x == False = 0

--Ejercicio 4

--a
limpiar :: String -> String -> String
limpiar [] x = x
limpiar (x:xs) (y) = limpiar xs (eliminar x y)

eliminar :: Char -> String -> String
eliminar _ [] = []
eliminar (x) (y:ys) | x == y = eliminar x ys
                    | x /= y = [y] ++ eliminar x ys

--b
difPromedio :: [Float] -> [Float]
difPromedio [] = []
difPromedio xs = restarACadaUno (promedio xs) xs

restarACadaUno :: Float -> [Float] -> [Float]
restarACadaUno _ [] = []
restarACadaUno n (x:xs) = [x-n] ++ restarACadaUno n xs 

promedio :: [Float] -> Float
promedio xs = suma xs / longitud xs

suma :: [Float] -> Float
suma [] = 0
suma (x:xs) = x + suma xs

longitud :: [Float] -> Float
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

--c
todosIguales :: [Int] -> Bool
todosIguales [] = True
todosIguales [x] = True
todosIguales(x:x2:xs) = x == x2 && todosIguales (x2:xs)

--Ejercicio 5

data AB a = Nil | Bin (AB a) a (AB a)

--a
vacioAB :: AB a -> Bool
vacioAB Nil = True
vacioAB (Bin _ x _) = False

--b
negacionAB :: AB Bool -> AB Bool
negacionAB Nil = Nil
negacionAB (Bin left root right) = (Bin (negacionAB left) (not root) (negacionAB right))

--c
productoAB :: AB Int -> Int
productoAB Nil = 1
productoAB (Bin left root right) = (productoAB left) * root * (productoAB right)
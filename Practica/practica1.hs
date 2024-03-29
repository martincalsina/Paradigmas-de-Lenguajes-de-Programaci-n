--Ejercicio 1

--b
normaVectorialCurry :: Float -> (Float -> Float)
normaVectorialCurry x = \y -> sqrt (x^2 + y^2)

parcialNorm = normaVectorialCurry 1
--probar parcialNorm 2 === devuelve 2.23

--c
substract :: Float -> (Float -> Float)
substract = flip (-) --ya está currificada

restar1 = substract 1
--probar restar1 5 === devuelve 4

--d
predecesor :: Float -> Float
predecesor = substract 1 --ya está currificada

--e
evaluarEnCero :: (Float -> Float) -> Float
evaluarEnCero = \f -> f 0 --ya está currificada

--f
dosVeces :: (Float -> Float) -> (Float -> Float)
dosVeces = \f -> (.) f f --ya está currificada

--g
flipAll :: [(Float -> Float -> Float)] -> [(Float -> Float -> Float)]
flipAll = map flip

--h
flipRaro :: Float -> (Float -> Float -> Float) -> Float -> Float
flipRaro = flip flip --está currificada

--pensar que flip :: (a -> b -> c) -> b -> a -> c
-- (a -> b -> c) es el primer parámetro y b el segundo
-- luego flip flip da vuelta a estos dos
-- b -> (a -> b -> c) -> a -> c

--Ejercicio 2

--a
myCurry :: ((a,b) -> c) -> a -> b -> c
myCurry f = \x -> (\y -> f (x,y))

sumaNoCurry (x, y) = x + y

sumaCurry = myCurry (sumaNoCurry)
--probar sumaCurry 1 2

--b
myUncurry :: (a -> b -> c) -> ((a,b) -> c)
myUncurry f = \(x, y) -> f x y  

sumaUncurry = myUncurry sumaCurry
--probar sumaUncurry (1, 2)

--c
-- ¿Se podría definir una función curryN, que tome una función de un número arbitrario de argumentos y
-- devuelva su versión currificada?

--myCurryN :: ((a, b) -> c) -> a -> b -> c
--myCurryN f = \x -> (\y -> myCurry (myCurryN f x) y)

--Ejercicio 3


--a

sumaFoldr :: Num a => [a] -> a
sumaFoldr = foldr (+) 0 

elemFoldr :: Eq a => a -> [a] -> Bool
elemFoldr e = foldr ((\x r -> (x == e) || r)) False

concatFoldr :: [a] -> [a] -> [a]
concatFoldr xs ys = foldr (:) ys xs --va agarrando el primer elemento de xs y lo concatena al tail de xs
-- si no hay tail en xs, lo concatena a ys y sigue con todo lo que tenía pendiente

filterFoldr :: (a -> Bool) -> [a] -> [a]
filterFoldr f = foldr (\x r -> if f x then x : r else r) []

mapFoldr :: (a -> b) -> [a] -> [b]
mapFoldr f = foldr (\x r -> f x : r) []

--b

--foldr1 toma los ultimos dos elementos de la lista y les aplica una función
mejorSegun :: (a -> a -> Bool) -> [a] -> a
mejorSegun f = foldr1 (\x y -> if f x y then x else y) 

--c ARREGLAR

--sumasParciales :: Num a => [a] -> [a]
--sumasParciales = tail (reverse . (foldr (\x (r:rs) -> x + (r) : (r:rs)) [0]))

--d

--sumaAlt :: Num a => [a] -> a
--sumaAlt = foldalt (+) (-) 0
--sumaAlt :: Num a => [a] -> a
--sumaAlt = foldr (\x (alt, acc) -> if alt then (not alt, x - acc) else (not alt, x + acc)) (False, 0)
sumaAlt :: Num a => [a] -> a
sumaAlt = foldr (-) 0

--e
sumaAltInverso :: Num a => [a] -> a
sumaAltInverso = sumaAlt . reverse

--Ejercicio 4

--a
permutaciones :: [a] -> [[a]]
permutaciones = foldr (\head acum -> (concatMap (\perm -> [ (take i perm) ++ [head] ++ (drop i perm) | i <- [0..(length perm)] ]) acum)) [[]]

--suponiendo que comenzamos con una lista vacía, y l es la lista de entrada, foldr recursivamente va a llegar hasta
--el último elemento de esta, llamémoslo l_n para comenzar a aplciar la función pasada
--así, l_n  será head y acum será [[]]
--se harán todas las permutaciones posibles con con el concatMap, que lo que hace es, dada la lista acumulada
--ir poniendo en cada uno de los lugares posibles a l_n y devolver una lista de eso.
--ahora se tiene [[l_n]]
--vamos con l_n-1, podemos ponerlo al principio o al final de la lista (take i perm o drop i perm), para i en rango válido
--de la permutación parcial que estamos viendo
--[[l_n, l_n-1], [l_n-1, l_n]]
--y así sigue

--b
partes :: [a] -> [[a]]
partes = foldr (\head acum -> (concatMap (\perm -> [perm, [head] ++ perm ]) acum)) [[]]
--partes = foldr (\x partes -> partes ++ map (x:) partes)
--          [[]]

--c

prefijos :: [a] -> [[a]]
--prefijos l = [(take i l) | i <- [0..(length l)]]
--prefijos = foldr (\x [acum] -> concat [(x:acum), (acum)]) [[]]
prefijos = foldl (\pref x -> pref ++ [(pref !! (length pref - 1)) ++ [x]]) [[]]

--partimos de la situación en que no tnemos nada [[]]
--pref es la lista de listas de prefijos, vacía por ahora, y x el elemento más a la derecha que todavía no hemos visto
--dada la lista de prefijos, accedemos a la última de todas, que es pref !! (lenght pref - 1) y le agregamos
-- nuestro elementos ++ [x], de tal manea que eso lo guardamos a la derecha del todos en la lista de listas pref
-- entonces, el último elemento siempre se lo va a copiar agregando a la derecha los números que todavía no hemos visto

--d CORREGIR
--sublistas :: [a] -> [[a]]
--sublistas xs = [] : filter (/= []) (prefijos (sufijos xs))

--sufijos :: [a] -> [[a]]
--sufijos = foldr (\x pref -> pref ++ [[x] ++ (pref !! (length pref - 1))]) [[]]

--Ejercicio 5

--Dadas las funciones,
--Indicar si la recursión utilizada en cada una de ellas es o no estructural. Si lo es, reescribirla utilizando foldr.
--En caso contrario, explicar el motivo.

elementosEnPosicionesPares :: [a] -> [a]
elementosEnPosicionesPares [] = []
elementosEnPosicionesPares (x:xs) = if null xs then [x] else x : elementosEnPosicionesPares (tail xs)

-- No es estructural, si bien el caso base es un valor fijo,
-- se está viendo a la cola de lista en el null xs, por fuera de la llamada recursiva, además,
-- la recursión no es sobre la cola, pues tail xs puede (y va a hacer casi siempre) distinto de xs´

----------------------------------------------------------------

entrelazar :: [a] -> [a] -> [a]
entrelazar [] = id
entrelazar (x:xs) = \ys -> if null ys then x : entrelazar xs [] else x : head ys : entrelazar xs (tail ys)

-- Es estructural
-- el caso base es fijo, dada una lista vacía devuelve la otra que le pasamos (identidad)
-- si la primera no es vacía, se ve que no se llama a xs fuera del caso recursivo

--foldrEntrelazar :: [a] -> [a] -> [a]
--foldrEntrelazar xs ys = foldr (\x r -> ([x] ++ [head (tail r)]) ++ r) ys xs

--Ejercicio 6

recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr _ z [] = z
recr f z (x : xs) = f x xs (recr f z xs)

--a

sacarUna :: Eq a => a -> [a] -> [a]
sacarUna e = recr (\x xs r -> if x == e then xs else (x:r)) []

--b

--Explicar por qué el esquema de recursión estructural (foldr) no es adecuado para implementar la función
--sacarUna del punto anterior

-- No es apropiado ya que es necesario hacer llamadas a xs por fuera de la recursión en sacarUna,
-- pues si el elemento buscado es encontrado, se debe devolver meramente xs (la cola de la sublista)
-- que estamos viendo en ese momento y terminar, el then xs en el recr

--c

insertarOrdenado :: Ord a => a -> [a] -> [a]
insertarOrdenado e = recr (\x xs r -> if e <= x then e:x:xs else x:r) [e]

--Ejercicio 7

--a
genLista :: a -> (a -> a) -> Int -> [a]
genLista init f n = foldr (\x ls -> if null ls then [init] else ls ++ [f (last ls)]) [] [1..n]

primerosNnumeros = genLista 1 (\x -> x + 1)

--b

desdeHasta :: Int -> Int -> [Int]
desdeHasta inicio fin = drop (inicio-1) (genLista 1 (\x -> x+1) fin)

--Ejercicio 8

--a
mapPares :: (a -> b -> c) -> [(a, b)] -> [c]
mapPares f = map (\(x, y) -> f x y) 

sumarVarios = mapPares (+) [(1, 2), (5, 2), (4, 2), (1,1)]

--b
armarPares :: [a] -> [b] -> [(a, b)]
armarPares = foldr (\x xs ys -> if null ys then [] else (x, head ys):xs (tail ys)) (const [])

--c
mapDoble :: (a -> b -> c) -> [a] -> [b] -> [c]
mapDoble f xs ys= mapPares f (armarPares xs ys)

sumarVarios2 = mapDoble (+) [1, 2, 3] [4, 5, 6]

--es el zipwith de Haskell
-- :t zipWith

--Ejercicio 9

--a
sumaMat :: [[Integer]] -> [[Integer]] -> [[Integer]]
sumaMat = foldr (\x xs ys -> if null ys then [] else (zipWith (+) x (head ys)):xs (tail ys)) (const [])

mat1 = [[1, 2, 3],
        [4, 5, 6],
        [7, 8, 9]]

mat2 = [[10, 11, 12],
        [13, 14, 15],
        [16, 17, 18]]

--b HACER

--trasponer :: [[Integer]] -> [[Integer]]

--Ejercicio 10

--Funciones que generan listas en base a un predicado y una función

--partiendo de una lista vacía, se tiene una función stop que nos dice cuando dejar de generar elementos
--a medida que vamos llenando a aquella.
--la función next es la que los va agregando mientras nos lo permita stop
generate :: ([a] -> Bool) -> ([a] -> a) -> [a]
generate stop next = generateFrom stop next []

--a partir de una lista cualquiera,
--se pone a agregar elementos hasta que ya salte la condición de stop xs
--en ese caso, pusimos un elemento de más, por lo que nos quedamos con init xs (todo menos el último elem)
--si no, a xs le concatenamos lo que nos diga que debe ser la función next xs
generateFrom:: ([a] -> Bool) -> ([a] -> a) -> [a] -> [a]
generateFrom stop next xs | stop xs = init xs
                          | otherwise = generateFrom stop next (xs ++ [next xs])

--a HACER
--generateBase :: ([a] -> Bool) -> a -> (a -> a) -> [a]
--generateBase stop base f = generate stop (\xs -> if null xs then [] else f (last xs)) 

--b

--c

--d

--Ejercicio 11

--SE PERMITE RECURSIÓN EXPLÍCITA PARA DEFINIR ESQUEMAS DE RECURSIÓN

--a

foldNat :: (Integer -> Integer -> Integer) -> Integer -> Integer -> Integer
foldNat f x 1 = x
foldNat f x n = f x (foldNat f x (n-1))



--a

--foldNat :: (Nat -> Nat -> Nat) -> Nat-> [Nat] -> Nat
--foldNat _ casoBase [] = casoBase
--foldNat fNats casoBase (x:xs) = fNats x (foldNat fNats casoBase xs)

--data Nat = Zero | Succ Nat

--sumaNat :: Nat -> Nat -> Nat
--sumaNat Zero x = x
--sumaNat (Succ y) x = Succ (sumaNat y x) 

--naturales :: [Nat]
--naturales = [Zero, Succ Zero, Succ (Succ Zero), Succ (Succ (Succ Zero)), Succ (Succ (Succ (Succ Zero))), Succ (Succ (Succ (Succ (Succ Zero))))]

--sumaDeNaturales = foldNat sumaNat Zero naturales

--b

potencia :: Integer -> Integer -> Integer
potencia x n = foldNat (\y res-> y * res) x n


--Ejercicio 12

data Polinomio a = X 
                  | Cte a 
                  | Suma (Polinomio a) (Polinomio a) 
                  | Prod (Polinomio a) (Polinomio a)

--X, Cte, Suma y Prod son de tipo Polinomio

evaluar :: Polinomio a -> a -> a
evaluar X valor = valor
evaluar (Cte a) valor = a
evaluar (Suma p1 p2) valor = (evaluar p1 valor) + (evaluar p2 valor)
evaluar (Prod p1 p2) valor = (evaluar p1 valor) * (evaluar p2 valor)


--Ejercicio 13

data AB a = Nil | Bin (AB a) a (AB a)

--Árbol binario

--a

-- b -> es el caso base, de arbol nulo
-- (AB a -> a -> AB a -> b) es una función que dados nodo izquierdo, raiz y nodo derecho devuelve algo
-- AB a es el árbol que recibimos

foldAB :: b -> (b -> a -> b -> b) -> AB a -> b
foldAB cNil cBin Nil = cNil
foldAB cNil cBin (Bin i r d) = cBin (foldAB cNil cBin i) r (foldAB cNil cBin d)

-- acá deriamos ser capaces de usar las "colas" del cada árbol por fuera del llamado recursivo
-- o sea, los subarboles izquierdo y derecho
-- en (AB -> a -> AB -> a -> AB a -> a -> AB a -> b) los primeros AB a -> AB a corresponden a ello

recAB :: b -> (AB a -> AB a -> b -> a -> b -> b) -> AB a -> b
recAb cNil cBin Nil = cNil
recAB cNil cBin (Bin i r d) = cBin i d (foldAB cNil cBin i) r (foldAB cNil cBin d)

--b

esNil :: AB a -> Bool
esNil Nil = True
esNil _ = False

altura :: AB a -> Integer
altura Nil = 0
altura (Bin i r d) = 1 + max (altura i) (altura d)

foldAltura :: AB a -> Integer
foldAltura = foldAB 0 (\left _ right -> 1 + max left right)

cantNodos :: AB a -> Integer
cantNodos Nil = 0
cantNodos (Bin i r d) = 1 + cantNodos i + cantNodos d

foldNodos :: AB a -> Integer
foldNodos = foldAB 0 (\left _ right -> 1 + left + right)

--c

esMejorSegun :: (a -> a -> Bool) -> AB a -> a
esMejorSegun f = foldAB Nil (\left r right -> elMejor f (elMejor f left r) right)

elMejor :: (a -> a -> Bool) -> a -> a -> a
elMejor _ Nil Nil = Nil
elMejor _ x Nil = x
elMejor _ Nil y = y
elMejor f x y = if f x y then x else y

arbolEjemplo :: AB Int
arbolEjemplo = Bin (Bin Nil 5 Nil) 10 (Bin (Bin Nil 8 Nil) 15 (Bin Nil 12 Nil))

maximoDeUnArbol :: Eq a => AB a -> a
maximoDeUnArbol = esMejorSegun (>=) 

--d

esABB :: AB a -> Bool
esABB = recAB True (\i d resleft r resright -> resleft && resright && esABBBasico i r d)

esABBBasico :: AB a -> a -> AB a -> Bool
esABBBasico Nil r Nil = True
esABBBasico Nil r (Bin _ d _) = r < d
esABBBasico (Bin _ i _) r Nil = r >= i
esABBBasico (Bin _ i _) r (Bin _ d _ )= r >= i && r < d

--e

--JUSTIFICACION DE LAS ELECCIONES DE ESTRUCTURAS DE RECURSION

--esNil: función muy básica, no requiere estructura

--altura: se usó fold porque no es necesario observar a los subárboles de un árbol dado para
--calcular la altura, sólo se los necesita usar en el llamado recursivo 

--cantNodos: igual que el anterior

--esMejorSegun: igual, sólo se va a usar a la raiz del subarbol en el que estemos parado y los resultados
--de los llamados recursivos

--esABB: ademas de ver que los llamados recursivos sean True, hay que ver que la raiz r de un arbol particular
--y sus nodos hijos i y d (que definen subarbols) cumplan el invariante de un ABB entre sí, por lo que hay que consular
--a aquellos dos fuera de la llamada recursiva.

--Ejercicio 14


--a

ramas :: AB a -> [[a]]
ramas = foldAB [[]] (\l r d -> [(r:(!!) l i) | i <- [0..(length l)]] ++ [(r:(!!) d i) | i <- [0..(length d)]]))

--con map
ramasMap :: AB a -> [[a]]
ramasMap = foldAB [[]] (\left r right -> map (r:) (left ++ right))

--pues no hace falta añadir r al camino parcial left y luego al right para recién ahí concatenarlos,
--en cualquier caso se lo debo agregar a todos

cantHojas :: AB a -> Integer
cantHojas = foldAB 0 (\left r right -> (1))

espejo :: AB a -> AB a
espejo = foldAB Nil (\left r right -> Bin right t left)

--b
--HACER
--mismaEstructura :: AB a -> Ab b -> Bool

--Ejercicio 15

data AIH a = Hoja a | Bin (AIH a) (AIH a)

--a

foldAIH :: (a -> b) -> (b -> b -> b) -> AIH a -> b
foldAIH fHoja fArbol (Hoja a) = fHoja a
foldAIH fHoja fArbol (Bin l d) = fArbol (foldAIH fHoja fArbol l) (foldAIH fHoja fArbol d)

--b

alturaAIH :: AIH a -> Integer
alturaAIH = foldAIH (\h -> 1) (\l d -> 1 + max l d) 

--es contar la cantidad de hojas
tamanioAIH :: AIH a -> Integer
tamanioAIH = foldAIH (\x -> 1) (\l d -> l + d) 

--Ejercicio 15 HACER

--Ejercicio 16

--a

data RoseTree a = Nil | Node a [RoseTree a]

--b

foldRoseTree :: b -> (a -> [b] -> b) -> RoseTree a -> b
foldRoseTree cNil cNode Nil = cNil
foldRoseTree cNil cNode (Node a b) = cNode a (foldRoseTree cNil cNode b) 


--c

--c.1

hojasRoseTree :: RoseTree a -> [a]
hojasRoseTree = foldRoseTree [] (\r leafs -> if null r then leafs else leafs ++ [r])

--c.2

--distanciasRoseTree :: RoseTree a -> [a]
--distanciasRoseTree foldRoseTree [] (\r leafs)

--NI IDEA

--c.3

alturaRoseTree :: RoseTree a -> Integer
alturaRoseTree = foldRoseTree 0 (\r alturas -> 1 + (maximum alturas))

doubleMe:: Int -> Int
doubleMe x = x + x

suma:: Int -> Int -> Int
suma x y = x + y

--Ejercicio 1:

f:: Int -> Int
f n | n == 1 = 8
    | n == 4 = 131
    | n == 16 = 16

g:: Int -> Int
g n | n == 8 = 16
    | n == 16 = 4
    | n == 131 = 1

h:: Int -> Int
h n = f (g n)

k:: Int -> Int
k n = g (f n)

--Ejercicio 2:

absoluto:: Int -> Int
absoluto n | n < 0 = -n
           | otherwise = n

maximoAbsoluto:: Int -> Int -> Int
maximoAbsoluto x y | absoluto x < absoluto y = absoluto y
                   | absoluto x > absoluto y = absoluto x
                   | absoluto x == absoluto y = absoluto x

maximo3:: Int -> Int -> Int -> Int
maximo3 x y z | x >= y && x >= z = x
              | y >= x && y >= z = y
              | z >= y && z >= x = z

algunoEs0:: Int -> Int -> Bool
algunoEs0 x y = (x == 0) || (y == 0)

ambosSon0:: Int -> Int -> Bool
ambosSon0 x y = (x == 0) && (y == 0)

mismoIntervalo:: Int -> Int -> Bool
mismoIntervalo x y | (x <= 3) && (y <= 3) = True
                   | (x > 3) && (x <= 7) && (y > 3) && (y <= 7) = True
                   | (x > 7) && (y > 7) = True
                   | otherwise = False

sumaDistintos:: Int -> Int -> Int -> Int
sumaDistintos x y z | x /= y && x /= z && y /= z = x + y + z
                    | x == y && x == z = 0
                    | x == y = z
                    | x == z = y
                    | z == y = x

esMultiploDe:: Int -> Int -> Bool
esMultiploDe x y = mod x y == 0

digitoUnidades:: Int -> Int
--mod entrega el resto y solo funciona con numeros positivos
digitoUnidades n | n >= 0 = mod n 10
                 | otherwise = mod (-n) 10

digitoDecenas:: Int -> Int
--div entrega el cociente de la division y solo funciona con numeros positivos
--abs devuelve el valor absoluto
digitoDecenas n = digitoUnidades (div (abs n) 10)

--Ejercicio 4:
todoMenor:: (Float, Float) -> (Float, Float) -> Bool
todoMenor (a1,b1) (a2,b2) | a1 < a2 && b1 < b2 = True
                          | otherwise = False

distanciaPuntos:: (Float, Float) -> (Float, Float) -> Float
distanciaPuntos (a1,b1) (a2,b2) = sqrt ((a2 - a1)^2 + (b2 - b1)^2)

sumaTerna:: (Float, Float, Float) -> Float
sumaTerna (x,y,z) = x + y + z

sumaSoloMultiplos:: (Int, Int, Int) -> Int -> Int
sumaSoloMultiplos (x,y,z) n | (mod x n == 0) && (mod y n == 0) && (mod z n == 0) = x + y + z
                            | (mod x n == 0) && (mod y n == 0) = x + y
                            | (mod x n == 0) && (mod z n == 0) = x + z
                            | (mod z n == 0) && (mod y n == 0) = z + y
                            | (mod x n == 0) = x
                            | (mod y n == 0) = y
                            | (mod z n == 0) = z

posPrimerPar:: (Int, Int, Int) -> Int
posPrimerPar (x,y,z) | (mod x 2 == 0) = 0
                     | (mod y 2 == 0) = 1
                     | (mod z 2 == 0) = 2
                     | otherwise = 4

crearPar:: a -> b -> (a, b)
crearPar a b = (a , b)

invertir:: (a , b) -> (b , a)
invertir (a , b) = (b , a)

--Ejercicio 5:

funF:: Int -> Int
funF n | (n <= 7) = (n^2)
       | otherwise = (2*n - 1)

funG:: Int -> Int
funG n | (mod n 2 == 0) = (div n 2)
       | otherwise = (3*n + 1)

todosMenores:: (Int, Int, Int) -> Bool
todosMenores (x,y,z) = (funF x > funG x) && (funF y > funG y) && (funF z > funG z)

--Ejercicio 6:

bisiesto:: Integer -> Bool
bisiesto n = not ( (mod n 4 /= 0) || ((mod n 100 == 0) && (mod n 400 /= 0)))

--Ejercicio 7:

distanciaManhattan:: (Float, Float, Float) -> (Float, Float, Float) -> Float
distanciaManhattan (a1,b1,c1) (a2,b2,c2) = abs (a2 - a1) + abs (b2 - b1) + abs (c2 - c1)

--Ejercicio 8:

comparar:: Integer -> Integer -> Integer
comparar x y | (sumaUltimosDosDigitos x) < (sumaUltimosDosDigitos y) = 1
             | (sumaUltimosDosDigitos x) > (sumaUltimosDosDigitos y) = -1
             | (sumaUltimosDosDigitos x) == (sumaUltimosDosDigitos y) = 0

sumaUltimosDosDigitos:: Integer -> Integer
sumaUltimosDosDigitos n = (mod (abs n) 10) + (mod (div (abs n) 10) 10)

--Ejercicio 9:



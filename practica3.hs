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

--algunoEs0:: Float -> Float -> Int

sumaDistintos:: Int -> Int -> Int -> Int
sumaDistintos x y z | x /= y && x /= z && y /= z = x + y + z
                    | x == y && x == z = 0
                    | x == y = z
                    | x == z = y
                    | z == y = x

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











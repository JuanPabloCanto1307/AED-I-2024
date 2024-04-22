import System.Console.Haskeline (Interrupt)
-- EJ 1

fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 2) + fibonacci (n - 1)

-- EJ 2 

parteEntera :: Float -> Int
parteEntera n | n < 1 = 0
              | n >= 1 = 1 + parteEntera (n - 1)

-- EJ 3

esDivisible :: Integer -> Integer -> Bool
esDivisible n d | n == 0 = True
                | abs d > abs n = False
                | otherwise = esDivisibleItera n d 1

esDivisibleItera :: Integer -> Integer -> Integer -> Bool
esDivisibleItera n d k | abs (d*k) > abs n = False
                       | d * k == n = True
                       | d * (-k) == n = True
                       | otherwise = esDivisibleItera n d (k+1)

-- EJ 4

--MAL ESTO
--sumaImparesM :: Integer -> Integer
--sumaImparesM n | n == 1 = 1
--              | mod n 2 == 1 = n + sumaImpares (n - 1)
--              | mod n 2 == 0 = sumaImpares (n - 1)

sumaImpares :: Integer -> Integer
sumaImpares n | n == 1 = 1
              | mod n 2 == 1 = n + sumaImpares (n - 2)
              | mod n 2 == 0 = n - 1 + sumaImpares (n - 3)
   
--sumaImparesItera

sumaImparesA :: Integer -> Integer
sumaImparesA 1 = 1
sumaImparesA n = sumaImpares (n-1) + 1 + (2*(n-1))

-- EJ 7

todosDigitosIguales :: Integer -> Bool
todosDigitosIguales n | n < 10 = True
                      | mod n 10 /= mod (div n 10) 10 = False
                      | otherwise = todosDigitosIguales (div n 10)

-- EJ 8

iesimoDigito :: Integer -> Integer -> Integer
iesimoDigito n i = mod (div n (10^(cantDigitos n - i))) 10

cantDigitos :: Integer -> Integer
cantDigitos n | n < 10 = 1
              | otherwise = 1 + cantDigitos (div n 10)

-- EJ 13

sumaNum :: Int -> Int -> Int
sumaNum n m | n == 1 = n^m
            | n > 1 = n^m + sumaNum (n - 1) m

sumaPot :: Int -> Int -> Int
sumaPot n m | m == 1 = sumaNum n m
            | m > 1 = sumaNum n m + sumaPot n (m - 1)

--EJ 16

menorDivisor :: Integer -> Integer
menorDivisor n = menorDivisorDesde n 2

menorDivisorDesde :: Integer -> Integer -> Integer
menorDivisorDesde n d | mod n d == 0 = d
                      | otherwise = menorDivisorDesde n (d + 1) --Para porque en el peor de los casos se divide por si mismo

esPrimo :: Integer -> Bool
esPrimo n = menorDivisor n == n

sonCoprimos :: Integer -> Integer -> Bool
sonCoprimos n m = esPrimo n && esPrimo m

nEsimoPrimo :: Integer -> Integer
nEsimoPrimo n | n == 1 = 0
              | n > 0 = compruebaPrimo n + nEsimoPrimo (n - 1)

compruebaPrimo :: Integer -> Integer
compruebaPrimo n | esPrimo n = 1
                 | otherwise = 0

--EJ 19


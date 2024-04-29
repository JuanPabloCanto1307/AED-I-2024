-- EJ 1.1
longitud :: [t] -> Integer
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

--EJ 1.2
ultimo :: [t] -> t
ultimo [x] = x
ultimo (x:xs) = ultimo xs

-- EJ 1.3
principio :: [t] -> [t]
principio [x] = []
principio (x:xs) = x : principio xs

-- EJ 1.4
reverso :: [t] -> [t]
reverso [] = []
reverso (x:xs) = ultimo (x:xs) : reverso (principio (x:xs))

-----------------------------------------------------------

-- EJ 2.1
pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False
pertenece n (x:xs) = n == x || pertenece n xs

-- EJ 2.2
todosIguales :: (Eq t) => [t] -> Bool
todosIguales [] = True
todosIguales [x] = True
todosIguales (x:xs) = pertenece x xs && todosIguales xs

-- EJ 2.3
todosDistintos :: (Eq t) => [t] -> Bool
todosDistintos [] = True
todosDistintos [x] = True 
todosDistintos (x:xs) = not (pertenece x xs) && todosDistintos xs

-- EJ 2.4
hayRepetidos :: (Eq t) => [t] -> Bool
hayRepetidos [] = False
hayRepetidos (x:xs) = pertenece x xs || hayRepetidos xs

-- EJ 2.5
quitar :: (Eq t) => t -> [t] -> [t]
quitar _ [] = []
quitar n (x:xs) | n == x = xs
                | n /= x = x:(quitar n xs)

-- EJ 2.6
quitarTodos :: (Eq t ) => t -> [t] -> [t]
quitarTodos _ [] = []
quitarTodos n (x:xs) | n == x = (quitarTodos n xs)
                     | n /= x = x:(quitarTodos n xs) 

-- EJ 2.7
eliminarRepetidos :: (Eq t) => [t] -> [t]
eliminarRepetidos [] = []
--eliminarRepetidos [x] = [x]
eliminarRepetidos (x:xs) = x : eliminarRepetidos (quitarTodos x xs) 

-- EJ 2.8
mismosElementos :: (Eq t) => [t] -> [t] -> Bool
mismosElementos [] [] = True
mismosElementos [x] [y] | x == y = True
mismosElementos (x:xs) (y:ys) = mismosElementosAux x (y:ys)
 
mismosElementosAux :: (Eq t) => t -> [t] -> Bool
mismosElementosAux n [x] | n /= x = False 
mismosElementosAux n (x:xs) | n /= x = mismosElementosAux n xs
                            | otherwise = True

------------------------------------------------------

-- EJ 3.1
sumatoria :: (Num t) => [t] -> t
sumatoria [] = 0
sumatoria [x] = x
sumatoria (x:xs) = x + sumatoria xs

-- EJ 3.2
productoria :: [Integer] -> Integer
productoria [] = 0
productoria [x] = x
productoria (x:xs) = x * productoria xs 

-- EJ 3.3 !!
maximo :: [Integer] -> Integer
maximo [x] = x
maximo (x:y:zs) | x > y = maximo (x:zs)
                | x <= y = maximo (y:zs)

-- EJ 3.4
sumarN :: Integer -> [Integer] -> [Integer]
sumarN n [x] = [n + x]
sumarN n (x:xs) = (n + x) : sumarN n xs 

-- EJ 3.5
sumarElPrimero :: [Integer] -> [Integer]
sumarElPrimero (x:xs) = sumarN x (x:xs)

-- EJ 3.6
sumarElUltimo :: [Integer] -> [Integer]
sumarElUltimo (x:xs) = sumarN (ultimo (x:xs)) (x:xs)

-- EJ 3.7
pares :: [Integer] -> [Integer]
pares [] = [] 
pares (x:xs) | mod x 2 == 0 = x:pares xs
             | otherwise = pares xs

-- EJ 3.8
multiplosDeN :: Integer -> [Integer] -> [Integer]
multiplosDeN n [] = []
multiplosDeN n (x:xs) | mod x n == 0 = x:multiplosDeN n xs
                      | otherwise = multiplosDeN n xs 

-- EJ 3.9
ordenar :: [Integer] -> [Integer]
ordenar [] = []
ordenar (x:xs) = ordenar (quitar (maximo (x:xs)) (x:xs)) ++ [maximo (x:xs)]

----------------------------------------------------------------------------

-- EJ 5.1
sumaAcumulada :: (Num t) => [t] -> [t]
sumaAcumulada (x:xs) = reverso (sumaAcumuladaAux (x:xs))

sumaAcumuladaAux :: (Num t) => [t] -> [t]
sumaAcumuladaAux [] = []
sumaAcumuladaAux (x:xs) = (sumatoria (x:xs)) : sumaAcumuladaAux (principio (x:xs))

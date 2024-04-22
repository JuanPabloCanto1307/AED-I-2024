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
--eliminarRepetidos :: (Eq t) => [t] -> [t]


-- EJ 3.3
maximo :: [Integer] -> Integer
maximo [x] = x
maximo (x:y:zs) | x > y = maximo (x:zs)
                | x <= y = maximo (y:zs)

-- EJ 3.9
ordenar :: [Integer] -> [Integer]
ordenar [] = []
ordenar (x:xs) = ordenar (quitar (maximo (x:xs)) (x:xs)) ++ [maximo (x:xs)]


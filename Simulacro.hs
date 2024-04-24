relacionesValidas :: [(String, String)] -> Bool
relacionesValidas [] = True
relacionesValidas [(x,y)] = True
relacionesValidas ((x,y):xs) | tuplasNoRepetidas (x,y) xs == False = False
                             | otherwise = relacionesValidas xs

tuplasNoRepetidas ::(String, String) -> [(String, String)] -> Bool
tuplasNoRepetidas (x,y) [] = True
tuplasNoRepetidas (x,y) ((j,k):xs) | (x,y)==(j,k) || (x,y)==(k,j) || x == y || j == k = False
                                   | otherwise = tuplasNoRepetidas (x,y) xs




personas :: [(String, String)] -> [String]
personas [] = ["nadie"]
personas [(x,y)] = desarmarTuplas [(x,y)]
personas ((x,y):xs) = desarmarTuplas ((x,y):xs)

desarmarTuplas :: [(String, String)] -> [String]
desarmarTuplas [(x,y)] = [x,y]
desarmarTuplas ((x,y):xs) = [x,y] ++ desarmarTuplas xs

--nombresSinRepetir :: String -> [String] -> [String]
--nombresSinRepetir x (y:ys) | x == y = ys 
--                           | otherwise = nombresSinRepetir y ys



--amigosDe :: String -> [(String, String)] -> [String]
--amigosDe "nadie" [] = ["nadie"]
-- esta mal. lo tenes que corregir



--personaConMasAmigos :: [(String, String)] -> String
--personaConMasAmigos [] = "yo" 
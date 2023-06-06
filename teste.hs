--1

contarSequencia :: String -> String
contarSequencia [] = []
contarSequencia (cabeca : cauda) = auxContarSequencia 0 cabeca (cabeca : cauda)

auxContarSequencia :: Int -> Char -> String -> String
auxContarSequencia qtd letra [] = letra : show qtd
auxContarSequencia qtd letra (cabeca : cauda)
    | letra == cabeca = auxContarSequencia (qtd+1) letra cauda
    | otherwise = letra : show qtd ++ auxContarSequencia 1 cabeca cauda


--2 Q

contador :: Eq a => a -> [a] -> Int
contador item lista
    | length lista == 0 = 0
    | item /= head lista = 0
    | otherwise = 1 + contador item (tail lista)
--    | head lista == item = 1 + contador item (tail lista)
--    | otherwise = contador item (tail lista)

organizador :: Ord a => [a] -> [(a, Int)]
organizador lista = quickSort (organizadorAUX lista (head lista) 1)

organizadorAUX :: Eq a => [a] -> a -> Int -> [(a, Int)]
organizadorAUX lista anterior aux
    | length lista == 0 = []
    | aux == 1 = [(head lista, contador (head lista) lista)] ++ organizadorAUX (tail lista) (head lista) 0
    | head lista == anterior = organizadorAUX (tail lista) (head lista) 0
    | otherwise = [(head lista, contador (head lista) lista)] ++ organizadorAUX (tail lista) (head lista) 0

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) =
    let smallerSorted = quickSort [a | a <- xs, a <= x]
        biggerSorted = quickSort [a | a <- xs, a > x]
    in smallerSorted ++ [x] ++ biggerSorted
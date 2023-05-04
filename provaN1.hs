--Função que retorna o enésimo número da sequência de Catalan
enesimoCatalan :: Int -> Int
enesimoCatalan x
    |x == 0 = 1
    |otherwise = div(fat(2*x)) ((fat(x+1))*(fat(x)))

--Função que verifica se um número pertence a sequência de Catalan
auxPertenceCatalan :: Int -> Int -> Bool
auxPertenceCatalan x y 
    |y == 0 = True
    |enesimoCatalan x == y = True
    |enesimoCatalan x > y = False
    |otherwise = auxPertenceCatalan (x+1) y


pertenceCatalan :: Int -> Bool
pertenceCatalan x  
    |x == 0 = True
    |otherwise = auxPertenceCatalan 0 x 

--Função que conta os números da sequência de catalan abaixo de um determinado valor de entrada

contaCatalan :: Int -> Int
contaCatalan x  
    |x == 0 = x
    |pertenceCatalan x == True = 1 + contaCatalan(x-1) 
    |otherwise = contaCatalan(x-1)

--Função que soma a sequencia que aparece em um intervalo de catalan
somaIntervaloCatalan :: Int -> Int -> Int
somaIntervaloCatalan x y
    |y == 0 = y
    |y == 1 = y+1
    |pertenceCatalan y == True && y >= x = y + somaIntervaloCatalan x (y-1)
    |otherwise = somaIntervaloCatalan x (y-1)

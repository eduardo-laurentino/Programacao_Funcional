--Função para somar dois números
soma :: Int -> Int -> Int
soma x y = x + y


--Função que gera o fatorial de um número
fat :: Int -> Int
fat x 
    | x == 0 = 1
    | x == 1 = 1
    | x == 2 = 2
    | otherwise = x*fat(x-1)

--Função que retorna a média entre dois números
media :: Double -> Double -> Double
media x y = (x + y) / 2 

--A função notaFinal chama a função media passando os dois valores e retoena a media.
notaFinal :: Double
notaFinal = media 5 3

--Função que retorna o enésimo número da sequência de fibonacci
fibonacci :: Int -> Int
fibonacci x 
    | x == 0 = 0
    | x == 1 = 1
    | x == 2 = 1
    | otherwise =  fibonacci(x-1) + fibonacci(x-2)

divisor :: Int -> Int -> Int
divisor x y
    |x < y = 0
    |x == y = 1
    |y == 1 = x
    |otherwise = 1 + divisor (x-y, y)
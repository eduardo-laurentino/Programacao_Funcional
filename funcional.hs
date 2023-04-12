--Função para somar dois números
soma :: Int -> Int -> Int
soma x y = x + y


--Função que gera o fatorial de um número
fat :: Int -> Int
fat x 
    |x == 0 = 1
    |x == 1 = 1
    |x == 2 = 2
    |otherwise = x*fat(x-1)

--Função que retorna a média entre dois números
media :: Double -> Double -> Double
media x y = (x + y) / 2 

--A função notaFinal chama a função media passando os dois valores e retoena a media.
notaFinal :: Double
notaFinal = media 5 3

--Função que retorna o enésimo número da sequência de fibonacci
fibonacci :: Int -> Int
fibonacci x 
    |x == 0 = 0
    |x == 1 = 1
    |x == 2 = 1
    |otherwise =  fibonacci(x-1) + fibonacci(x-2)

--Função que retorna o menor entre dois números
menorDeDois :: Int -> Int -> String
menorDeDois x y
    |x > y = "X e maior"
    |x < y = "Y e maior"
    |otherwise = "Sao Iguais"

--Função que retorna a quantidade de algarismos de um número
qtdAlgarismos :: Int -> Int
qtdAlgarismos x
    |x < 10 = 1
    |otherwise = 1 + qtdAlgarismos(div x 10)

--Função que retorna o resto da divisão
modulo :: Int -> Int -> Int
modulo x y = mod x y


--Função que verifica se um número é par
par :: Int -> Bool
par x 
    |modulo x 2 == 0 = True
    |otherwise = False

--Função que verifica se um número é primo
primo :: Int -> Bool
primo x 
    |x == 1 = False
    |x == 2 = True
    |x > 2 && par x == True = False
    |x > 2 && par x == False && modulo x 3 /= 0 && modulo x 5 /= 0 && modulo x 7 /= 0 = True


--Função que retorna se um número é coprimo
coprimo :: Int -> Int -> Int -> Bool
coprimo n d i 
    |i == 1 = True
    |mod n i == 0 && mod d i == 0 = False
    |otherwise = coprimo n d i-1


--Função que verifica se um número é primo de sofí german
primoSG :: Int -> Bool
primoSG n
    |primo n == True && primo (2*n+1) == True = True
    |otherwise = False


--Função que retorna um número da sequência de Padovan
padovan :: Int -> Int
padovan x 
    |x <= 2 = 1
    |otherwise = padovan(x - 2) + padovan(x - 3)


--Função que retorna a soma dos digitos de um número
somaDigitos :: Int -> Int
somaDigitos x
    |x < 10 = x
    |otherwise = (mod x 10) + somaDigitos(div x 10)
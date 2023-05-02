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

--Função que retorna a soma dos fatoriais
somaFat :: Int -> Int
somaFat x 
    |x == 0 = 1
    |otherwise = somaFat(x-1) + fat(x)

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
aux :: Int -> Int -> Bool
aux x y 
    |y == 1 = True
    |mod x y == 0 = False
    |otherwise = aux x(y-1)

primo :: Int -> Bool
primo x 
    |x == 1 = False
    |x == 2 = True
    |otherwise = aux x(x-1)


--Função que retorna se um número é coprimo
coprimo :: Int -> Int -> Int -> Bool
coprimo n d i 
    |i == 1 = True
    |modulo n i == 0 && modulo d i == 0 = False
    |otherwise = coprimo n d (i-1)


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

--Função que retorna o0 resultado de uma potência
potencia :: Int -> Int ->  Int
potencia x y
    |x == 0 && y /= 0 = x
    |y == 0 = 1
    |otherwise = x ^ y


veja :: Int->IO ()
veja n  
    |n > 1000 = putStrLn"Maior que 1000"
    |n > 100 && n < 1000 = putStrLn"Entre 100 e 1000"
    |n < 100 = putStrLn"Menor que 100"    
    |n == 1000 = putStrLn"Igual a 1000"
    |n == 100 = putStrLn"Igual a 100"

--Função que retorna o valor de uma venda
venda :: Int -> Int
vanda 0 = 5
venda 1 = 6
venda 2 = 4
venda 3 = 8
venda 4 = 20
venda 5 = 3
venda 6 = 9
venda 7 = 10

--Função que retorna o total de vendas
totalVendas :: Int -> Int
totalVendas n 
    |n == 0 = 5
    |otherwise = totalVendas(n-1)+venda(n)

--Função que retorna a maior venda
maiorVenda :: Int -> Int
maiorVenda n
    |n == 0 = venda 0
    |venda n > maiorVenda(n-1) = venda n 
    |otherwise = maiorVenda(n-1)

--Função lógica nand
nand :: Bool -> Bool -> Bool
nand n m
    | n == True && m == True = False
    | otherwise = True

--Função que retorna a quantidade de números iguais
qtdNumerosIguais :: Int -> Int -> Int -> Int
qtdNumerosIguais n m p 
    |n == m && n == p && m == p = 3
    |n == m || n == p || m == p = 2
    |otherwise = 0

--Função que verifica se os números são primos gêmeos
primosGemeos :: Int -> Int -> Bool
primosGemeos x y 
    |x - y == 2 || x - y == -2 && primo(x) == True && primo(y) == True = True
    |otherwise = False

--Função que verifica se um número pertence a um par de primos gêmeos
pertencePrimosGemeos :: Int -> Bool
pertencePrimosGemeos x 
    |primo(x+2) || primo(x-2) && primo(x) == True = True
    |otherwise = False

--Função que retorna a quantidade de pares primos gêmeos
contaPrimosGemeos :: Int -> Int
contaPrimosGemeos x 
    |x < 3 = 0
    |primosGemeos(x - 2) x = 1 + contaPrimosGemeos(x-1)
    |otherwise = contaPrimosGemeos(x-1)

--Função que retorna a soma dos pares primos gêmeos
somaParGemeos :: Int -> Int
somaParGemeos x 
    |x < 3 = 0
    |primosGemeos(x - 2) x = x + (x - 2) + somaParGemeos(x-1)
    |otherwise = somaParGemeos(x-1)

--Função que retorna se um número é impar
impar :: Int -> Bool
impar x 
    |modulo x 2 == 0 = False
    |otherwise = True

--Função que soma os algarismos pares de um número
somaAlgarismosPar :: Int -> Int
somaAlgarismosPar x 
    |x < 10 && par(x) = x
    |x < 10 && not (par(x)) = 0
    |par(modulo x 10) = (modulo x 10) + somaAlgarismosPar(div x 10) 
    |otherwise = somaAlgarismosPar(div x 10)

--Função que multiplica os algarismos primos de um número
multAlgarismosPrimo :: Int -> Int
multAlgarismosPrimo x 
    |x < 10 && primo x = x 
    |x < 10 && not(primo x) = 1
    |primo(modulo x 10) = (modulo x 10) * multAlgarismosPrimo(div x 10)
    |otherwise = multAlgarismosPrimo(div x 10)

--Função que conta os algarismos primos de um número
contaAlgarismosPrimos :: Int -> Int
contaAlgarismosPrimos x 
    |x < 10 && primo x = 1
    |x < 10 && not(primo x) = 0
    |primo(modulo x 10) = 1 + contaAlgarismosPrimos(div x 10)
    |otherwise = contaAlgarismosPrimos(div x 10)

--Função que retorna o enésimo primo da sequência de fibonacci
auxEnesimoFibonacci::Int->Int->Int->Int
auxEnesimoFibonacci n p s
    |(n==1) && primo s = s
    |primo s = auxEnesimoFibonacci (n-1) s (p+s)
    |otherwise= auxEnesimoFibonacci n s (p+s)

enesimoPrimoFibonacci::Int->Int
enesimoPrimoFibonacci n = auxEnesimoFibonacci n 1 1

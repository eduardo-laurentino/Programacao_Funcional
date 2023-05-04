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

modulos :: Int -> Int -> Int
modulos x y 
    |x == y = 0
    |x >= y = modulos(x-y) y
    |otherwise = abs(x-y)   

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


veja :: Int -> IO ()
veja n  
    |n > 1000 = putStrLn"Maior que 1000"
    |n > 100 && n < 1000 = putStrLn"Entre 100 e 1000"
    |n < 100 = putStrLn"Menor que 100"    
    |n == 1000 = putStrLn"Igual a 1000"
    |n == 100 = putStrLn"Igual a 100"

--Função que retorna o valor de uma venda
venda :: Int -> Int
venda 1 = 0
venda 2 = 4
venda 3 = 8
venda 4 = 20
venda 5 = 3
venda 6 = 9
venda 7 = 10
venda _ = 1

--Função que retorna o total de vendas
totalVendas :: Int -> Int
totalVendas x 
    |x == 0 = 5
    |otherwise = totalVendas(x-1)+venda(x)

--Função que retorna a maior venda
maiorVenda :: Int -> Int
maiorVenda x
    |x == 0 = 0
    |venda x > maiorVenda(x-1) = venda x 
    |otherwise = maiorVenda(x-1)

--Função que retorna a semana sem vendas
--Retorna 0 se todas semanas tivrem vendas
--Se for informado uma semana que não está na lista vendas ela será considerada uma semana sem vendas
semanaSemVenda :: Int -> Int
semanaSemVenda x 
    |x < 0 = 0
    |venda x == 0 = x
    |otherwise = semanaSemVenda(x-1)

--Função que verifica se existe uma sem venda
existeSemanaSemVenda :: Int -> Bool
existeSemanaSemVenda x 
    |x < 0 = False
    |venda x == 0 = True
    |otherwise = existeSemanaSemVenda(x-1)

--Função que retorna a maior semana que teve a maior venda
maiorSemanaMaiorVenda :: Int -> Int
maiorSemanaMaiorVenda x 
    |x == 0 = 0
    |venda x > venda(maiorSemanaMaiorVenda(x-1)) = x 
    |otherwise = maiorSemanaMaiorVenda(x-1)

--Função lógica nand
nand :: Bool -> Bool -> Bool
nand n m
    |n == True && m == True = False
    |otherwise = True

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

enesimoPrimoFibonacci::Int -> Int
enesimoPrimoFibonacci n = auxEnesimoFibonacci n 1 1

--Função que retorna o número que gera o fatorial informado
auxGeraFatorial :: Int -> Int -> Int
auxGeraFatorial y x
    |fat y == x = y
    |otherwise = auxGeraFatorial (y+1) x

geraFatorial :: Int -> Int
geraFatorial n 
    |n == 1 = 1
    |otherwise = auxGeraFatorial 2 n

--Função que retorna se um número é perfeito ou não
somaDivisores :: Int -> Int -> Int
somaDivisores x y 
    |y == 1 = 1
    |modulo x y == 0 = y + somaDivisores x (y-1)
    |otherwise = somaDivisores x (y-1)


--Função que verifica se um número é perfeito
perfeito :: Int -> Bool
perfeito x 
    |x == 1 = False
    |somaDivisores x (x-1) == x = True
    |otherwise = False

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
    |pertenceCatalan y == True = y + somaIntervaloCatalan x (y-1)
    |otherwise = somaIntervaloCatalan x (y-1)

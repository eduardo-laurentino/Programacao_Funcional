--Função para somar dois números
soma :: Int -> Int -> Int
soma x y = x + y

--Função que gera o fatorial de um número
fat :: Int -> Int
fat x 
    |x == 0 = 1
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

--Função que verifica se um número pertence a sequência de fibonacci
auxFibonacci :: Int -> Int -> Bool
auxFibonacci x y
    |x == 0 || x == 1 = True
    |fibonacci y == x = True
    |fibonacci y > x = False
    |otherwise = auxFibonacci x (y+1)

pertenceFibonacci :: Int -> Bool
pertenceFibonacci x 
    |x == 0 = True
    |otherwise = auxFibonacci x 0

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
    |x > y = modulos(x-y) y
    |otherwise = x

--Função que retorna o valor de uma potência
poten :: Int -> Int -> Int
poten x y 
    |y == 0 = 1
    |otherwise = x*(poten x (y-1))

--Função que verifica se um número é par
par :: Int -> Bool
par x 
    |modulo x 2 == 0 = True
    |otherwise = False

--Função que calcula o valor absoluto de um  número inteiro
valorAbsoluto :: Int -> Int
valorAbsoluto x = if x >= 0 then x else -x 

--Função que retorna a divisão inteira entre dois números
divisao :: Int -> Int -> Int
divisao x y 
    |x < y = 0
    |x == y = 1
    |y == 1 = x 
    |otherwise = 1 + divisao(x-y) y 

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

--Função que multiplica dois números
multiplicacao :: Int -> Int -> Int
multiplicacao x y 
    |x == 0 || y == 0 = 0
    |y > 0 = x + multiplicacao x (y-1)
    |otherwise = negate (multiplicacao x (negate y))

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
    |x == 0 = False
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
    |y == 1 = 2
    |pertenceCatalan y == True && y >= x = y + somaIntervaloCatalan x (y-1)
    |otherwise = somaIntervaloCatalan x (y-1)

--Função que retorna o produto dos números da sequẽncia de fibonacci até um X informado
produtoFibonacci :: Int -> Int
produtoFibonacci x 
    |x == 1 = 1
    |x == 2 = 1
    |otherwise = fibonacci(x) * produtoFibonacci(x-1)

--Função que retrna o enésimo número de uma PG
-- a1 -> número inicial da PG
-- q -> razão da PG
-- n -> enésimo termo da PG
enesimoPg :: Int -> Int -> Int -> Int
enesimoPg a1 q n 
    |a1 == 0 = 0
    |q == 0 = 0
    |q == 1 = a1
    |otherwise = a1 * q^(n-1)

--Problema dos dias de coleta
diaColeta :: Int -> Int
diaColeta x 
    |x == 1 = 7
    |otherwise = diaColeta(x-1) + 7 * x

--Conjectura de Goldbach - todo número par maior que 2 é a soma de dois números primos
auxGoldbach :: Int -> Int -> (Int, Int)
auxGoldbach x y 
    |primo(y) == True && primo(x-y) == True = (y, (x-y))
    |otherwise = auxGoldbach x (y+1)

goldbach :: Int -> (Int, Int)
goldbach x = auxGoldbach x 1

--Função totiente de Euler -> é a contagem dos números cujo maior divisor comum com a entrada x é 1
totiente :: Int -> Int -> Int
totiente x y 
    |y == 1 = 1
    |coprimo x y y == True = 1 + totiente x (y-1)
    |otherwise = totiente x (y-1)

contTotiente :: Int -> Int
contTotiente x = totiente x (x-1)

--Função que verifica o maior entre três números
max3 :: Int -> Int -> Int -> Int
max3 numero1 numero2 numero3 = if numero1 > numero2 && numero1 > numero3
    then numero1
    else if numero2 > numero3 then numero2
    else numero3

--Reursividade mútua
ePar :: Int -> Bool
ePar x 
    |x == 0 = True
    |x > 0 = eImpar(x-1)
    |otherwise = ePar(-x)

eImpar :: Int -> Bool
eImpar x 
    |x == 0 = False
    |x > 0 = par (x-1)
    |otherwise = impar (-x)

--              TIPOS ESTRUTURADOS N2
--Função que valida a hora informada
--Cria o tipo hora
type Hora = (Int, Int, Int)
valida :: Hora -> Bool
valida (h, m, s)
    |h < 0 || h > 23 = False
    |m < 0 || m > 59 = False
    |s < 0 || s > 59 = False
    |otherwise = True

--Função que calcula o total de segundos da hora informada
totalSegundos :: Hora -> Int
totalSegundos (h, m, s) = s + m*60 + h*60*60

--Função que converte os segundos em horas
converteSegundos :: Int -> Hora
converteSegundos total = (horas, minutos, segundos)
    where
        horas = div total 3600
        minutos = div (mod total 3600) 60
        segundos = mod (mod total 3600) 60

--Função que calcula a diferença entre duas horas informadas  
diferencaHora :: Hora -> Hora -> Hora
diferencaHora hora1 hora2
    |totalSegundos(hora1) > totalSegundos(hora2) = converteSegundos (totalSegundos(hora1)-totalSegundos(hora2))
    |otherwise = converteSegundos (totalSegundos(hora2)-totalSegundos(hora1))

--Função que retorna verdadeiro se o ano informado é bissexto e falso se não for bissexto
anoBissexto :: Int -> Bool
anoBissexto ano = (mod ano 4 == 0) && not (mod ano 100 == 0) || (mod ano 400 == 0)

--Cria o tipo data
type Data = (Int, Int, Int)
--Função que retorna verdadeiro se a data informada for válida e falso se a data for inválida
validaData :: Data -> Bool
validaData (dia, mes, ano)
    |dia < 1 || dia > 31 = False
    |mes < 1 || mes > 12 = False
    |mes == 4 || mes == 6 || mes == 9 || mes == 11 && dia > 30 = False
    |mes == 2 && not (anoBissexto(ano)) && dia > 28 = False
    |otherwise = True


--                      LISTAS

--Função que soma os valores de uma lista
somaLista :: [Int] -> Int
somaLista [] = 0
somaLista(cabeca:cauda) = cabeca + somaLista cauda

--Função que inverte uma sequência de caracteres
inverte :: [Char] -> [Char]
inverte [] = []
inverte (a:as) = inverte as ++ [a]

--Função que verifica se duas lista são iguis
listasIguais :: [Int] -> [Int] -> Bool
listasIguais x y 
    |[x] == [y] = True
    |otherwise = False

--Função que ordena uma lista de inteiros
ordena :: [Int] -> [Int]
ordena [] = []
ordena (x: xs) = insere x (ordena xs)

insere :: Int -> [Int] -> [Int]
insere x[] = x:[]
insere x (cabeca:cauda)
    |x <= cabeca = x:cabeca:cauda
    |otherwise = cabeca:(insere x cauda)

--                  Expressões ZF
--Função que recebe uma lista retorna outra lista de números pares multiplicados por 2
geradores :: [Int] -> [Int]
geradores x = [2*a | a <- x, mod a 2 == 0]

--Função que recebe uma tupla e soma os valores de cada tupla
somaTupla :: [(Int, Int)] -> [Int]
somaTupla pares = [a + b | (a, b) <- pares]

geradorDuplo :: [Int] -> [Int] -> [(Int, Int)]
geradorDuplo x y = [(a, b) | a <- x, b <- y]

--Função que retorna uma lista de divisores de um número
zfDivisores :: Int -> [Int]
zfDivisores x = [a | a <- [1..(x-1)], mod x a == 0]

--Função que retorna uma lista de números perfeitos
zfPerfeitos :: Int -> Bool
zfPerfeitos n = sum(zfDivisores n) == n

perfect :: Int -> [Int]
perfect n = [x | x <- [1..n], zfPerfeitos x]

--Função que concatena listas
zfConcatena :: [[Int]] -> [Int]
zfConcatena x = [x | sub <- x, x <-sub]

--Função que retorna os números ímpares entre 1 e 100
zfImpar = [x | x <- [2 .. 99], impar x]

--Função que retorna os números pares entre 10 e 100
zfPar = [x | x <- [11 .. 99], par x]

--Função que retorna os números ímpares entre um 1 e X
imparN :: Int -> [Int]
imparN x = [x | x <- [1 .. x], impar x]

--Função que retorna os números entre 1 e X que são múltiplos de 3 e 5
zfMultiplos :: Int -> [Int]
zfMultiplos x = [x | x <- [1..x], mod x 3 == 0 && mod x 5 == 0]

--Função que retorna uma lista de tuplas de 1 a x contendo x e o seu respectivo quadrado
tuplaQuadrado :: Int -> [(Int, Int)]
tuplaQuadrado x = [(x, x*x) |x <- [1..x]]
--Função que retorna tuplas com índices de uma matriz 3 x 4
tuplaMatriz = [(x, y) | x <- [0..3], y <- [0..4]]

--Função que retorna uma matriz n x m
matriz :: Int -> Int -> [(Int, Int)]
matriz n m = [(n, m) | n <- [0..n], m <- [0..m]]

--Função que retorna uma lista dos n primeiros números da sequência de fibonacci
listaFibonacci :: Int -> [Int]
listaFibonacci x = [x | x <- [0..x], pertenceFibonacci x == True]

--Função que replica um caractere inserido de acordo com o valor informado
replicaChar :: Char -> Int -> [Char]
replicaChar caractere numero 
    |numero == 0 = []
    |otherwise = caractere: replicaChar caractere(numero-1)

--método de ordenação Quick Sort
quickSort :: [Int] -> [Int]
quickSort [] = []
quickSort (a:x) = quickSort [y | y <- x, y <= a] ++[a]++ quickSort [y | y <- x, y > a]

--Função que recebe dois números inseridos pelo terminal e retorna a soma
main :: IO ()
main = do
putStrLn "Digite um numero:"
s1 <- getLine
putStrLn "Digite outro numero:"
s2 <- getLine
putStr "Soma dos numeros digitados: "
let n1 = read s1 :: Double
let n2 = read s2 :: Double
putStrLn (show (n1 + n2))


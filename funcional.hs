import Data.Char 
import Data.String
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

--Função que verifica se um número pertence a uma lista
pertenceLista :: [Int] -> Int -> Bool
pertenceLista lista numero
    |lista == [] = False
    |head lista == numero = True
    |otherwise = pertenceLista(tail lista) numero

--Função que retorna o maior número de uma lista
auxMaiorNumLista :: [Int] -> Int -> Int
auxMaiorNumLista lista aux
    |tail lista == [] = aux
    |head lista > aux = auxMaiorNumLista (tail lista) (head lista)
    |otherwise = auxMaiorNumLista (tail lista) aux

maiorNumLista :: [Int] -> Int
maiorNumLista lista
    |lista == [] = 0
    |otherwise = auxMaiorNumLista lista 0

--Função que recebe um número e retorna o enésimo número da lista
enesimoNumLista :: [Int] -> Int -> Int
enesimoNumLista lista numero
    |lista == [] = 0
    |length lista < numero = 0
    |otherwise = (lista !! numero)

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

--Função que remove o enésimo número de uma lista
removeEnesimoLista :: Int -> [Int] -> [Int]
removeEnesimoLista enesimo lista
    |lista == [] = []
    |otherwise = remove enesimo 0 [] lista
    
remove :: Int -> Int -> [Int] -> [Int] -> [Int]
remove num aux novalst lst
    | lst == [] = []
    | num == aux = reverse(novalst) ++ (tail lst)
    | otherwise = remove num (aux+1) ((head lst):novalst) (tail lst)

--Função que verifica se uma frase é palíndromo
inverteString :: String -> String
inverteString "" = ""
inverteString (a:as) = inverteString as ++ [a]

palindromo :: String -> Bool
palindromo nome 
    |nome == "" = False
    |nome == (inverteString nome) = True
    |otherwise = False

--Função que conta a quantidade de caracteres de uma frase
contaCaractere :: String -> Int
contaCaractere frase = auxcontaCaractere frase 0

auxcontaCaractere :: String -> Int -> Int
auxcontaCaractere frase x 
    |frase == "" = x 
    |otherwise = auxcontaCaractere (tail frase) (x+1)

--Função que conta as ocorrências de um determinado caractere em uma string
contar :: Char->String->Int
contar c str
   |str == "" = 0
   |c == (head str) = 1+ contar c (tail str)
   |otherwise = contar c (tail str)

--Função que retorna uma lista com as palavras que tenha tamanho maior ou igual ao inteiro informado
auxiliarFiltro :: Int -> [String] -> [String] -> [String]
auxiliarFiltro tamanho lista listaFinal
    |lista == [] = listaFinal
    |length (head lista) >= tamanho = auxiliarFiltro tamanho (tail lista) (reverse(head lista:listaFinal))
    |otherwise = auxiliarFiltro tamanho (tail lista) listaFinal

filtroTamanho :: Int -> [String] -> [String]
filtroTamanho tamanho lista = auxiliarFiltro tamanho lista []

retornaPalavra :: String -> String -> String
retornaPalavra texto novoTexto
    |texto == [] = []
    |head texto == ',' || head texto == ' ' = reverse(novoTexto)
    |otherwise = retornaPalavra( tail texto) (head texto:novoTexto)

--Função que converte uma string binária em string hexadecimal
conv "0000" = "0"
conv "0001" = "1"
conv "0010" = "2"
conv "0011" = "3"
conv "0100" = "4"
conv "0101" = "5"
conv "0110" = "6"
conv "0111" = "7"
conv "1000" = "8"
conv "1001" = "9"
conv "1010" = "A"
conv "1011" = "B"
conv "1100" = "C"
conv "1101" = "D"
conv "1110" = "E"
conv "1111" = "F"

bin2hex::String->String
bin2hex s
   |s=="" = ""
   |mod (length s) 4 /=0 = bin2hex((replicate (4-(mod (length s) 4)) '0') ++ s)
   |otherwise = conv (take 4 s) ++ bin2hex(drop 4 s)

--Função que elimina uma sequência de caracteres repetidos de uma string
eliminaCaractere :: String -> String
eliminaCaractere frase = elimina frase ""

elimina :: String->String->String
elimina lista final
   |lista=="" = final
   |final=="" = elimina (tail lista) [head lista]
   |head lista == last final = elimina (tail lista) final
   |otherwise = elimina (tail lista) (final++[head lista])

--Função que duplica os elementos de uma lista
duplicaLista :: [Int] -> [Int]
duplicaLista [] = []
duplicaLista(cabeca:cauda) = cabeca:cabeca: duplicaLista(cauda)

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

--Função que retorna uma lista da sequência de fibonacci até n
listaFibonacci :: Int -> [Int]
listaFibonacci x = [x | x <- [0..x], pertenceFibonacci x == True]

--Função que retorna uma lista dos n primeiros números da sequência de fibonacci
primeiroFibonacci :: Int -> [Int]
primeiroFibonacci x = [fibonacci x | x <- [0..x]]

--Função que replica um caractere inserido de acordo com o valor informado
replicaChar :: Char -> Int -> [Char]
replicaChar caractere numero 
    |numero == 0 = []
    |otherwise = caractere: replicaChar caractere(numero-1)

--Função que resolve o problema da torre de Hanoi
hanoi :: Int -> Int -> Int -> Int -> [String]
hanoi discos origem auxiliar destino
    |discos == 0 = []
    |discos == 1 = [show(origem) ++ "->" ++ show(destino)]
    |otherwise = ch1 ++ [show(origem) ++ "->" ++ show(destino)] ++ ch2
        where
            ch1 = hanoi(discos-1) origem destino auxiliar
            ch2 = hanoi(discos-1) auxiliar origem destino

--Função que elimina os caracteres repitidos de uma string
compressaoElimina :: String -> String -> String
compressaoElimina lista listaFinal
    |lista == [] = listaFinal
    |listaFinal == [] = compressaoElimina (tail lista) [head lista]
    |head lista == last listaFinal = compressaoElimina (tail lista) listaFinal
    |otherwise = compressaoElimina (tail lista) (listaFinal ++ [head lista])

--Função que codifica uma mensagem pelo método da cifra de cesar
codifica :: Int -> Char -> Char
codifica x c
    |(ord c) + x < 65 = chr (90 + (ord c) - 64 + x)
    |(ord c) + x > 90 = chr (64 + (ord c) - 90 + x)
    |ord c >= 64 && ord c < 91 = chr ((ord c) + x)
    |otherwise = c

cifraCesar :: Int -> String -> String
cifraCesar x mensagem
    |mensagem == "" = ""
    |x == 0 = mensagem
    |otherwise = codifica x (head mensagem) : cifraCesar x (tail mensagem)

--método de ordenação Quick Sort
quickSort :: [Int] -> [Int]
quickSort [] = []
quickSort (a:x) = quickSort [y | y <- x, y <= a] ++[a]++ quickSort [y | y <- x, y > a]

--Função que comprime e conta os caracteres repetidos de uma string
compressao :: String -> String
compressao "" = ""
compressao (cabeca:cauda)
    |cauda == [] = [cabeca]
    |cabeca /= head cauda = cabeca:(compressao cauda)
    |otherwise = cabeca:(show cont)++(compressao (drop cont(cabeca:cauda)))
    where
        cont = contaiguaisinicio cabeca (cabeca:cauda)

contaiguaisinicio :: Char -> String -> Int
contaiguaisinicio _ [] = 0
contaiguaisinicio c (cabeca:cauda)
    |c == cabeca = 1+ contaiguaisinicio c cauda
    |otherwise = 0

-- Funçao que conta a quantidade de repetições de uma palavra em uma lista
entradaDados :: [String] -> [(String, Int)]
entradaDados palavra = processamento palavra []

processamento :: [String] -> [String] -> [(String, Int)]
processamento palavra aux
    |palavra == [] = []
    |pertenceListaDados (head palavra) aux == True = processamento (tail palavra) aux
    |otherwise = auxProcessamento (head palavra) palavra 0 ++ processamento (tail palavra)(head palavra:aux)

auxProcessamento :: String -> [String] -> Int -> [(String, Int)]
auxProcessamento palavra listaPalavra aux
    |listaPalavra == [] = [(palavra, aux)]
    |head listaPalavra == palavra = auxProcessamento palavra (tail listaPalavra) (aux+1)
    |otherwise = auxProcessamento palavra (tail listaPalavra) aux

pertenceListaDados :: String -> [String] -> Bool
pertenceListaDados palavra listaPalavra
    |listaPalavra == [] = False
    |palavra == head listaPalavra = True
    |otherwise = pertenceListaDados palavra (tail listaPalavra)

-- Função que recebe um texto e conta as palavras repetidas do texto
dados :: String -> [(String, Int)]
dados lista 
    |lista == [] = []
    |otherwise = processamento (analise lista "" []) []

analise :: String -> String -> [String] -> [String]
analise frase palavra listaVazia
    |frase == [] = reverse(reverse(palavra):listaVazia)
    |head frase == ' ' = analise (tail frase) "" (reverse(palavra):listaVazia)
    |otherwise = analise (tail frase) (head frase:palavra) listaVazia

--                  POLIMORFISMO
--Função polimórfica que filtra as palvras de uma lista maiores ou iguais a um determinado n 
filtroLista :: Int -> [[a]] -> [[a]]
filtroLista n lista = [p | p <- lista, length p >= n]

--Função que verifica se dois números são iguais
polimorfismo :: (Num a, Eq a) => a -> a -> Bool
polimorfismo x y
    |x == y = True
    |otherwise = False

-- Fução que forma pares com números de duas listas
fazPar :: [t] -> [u] -> [(t,u)]
fazPar n m = [ (a, b) | a <- n, b <- m]

-- Função que retorna uma tupla de listas; uma lista com os primeiros elementos da tupla e a outra com os segundos elementos
unZip::[(u,v)]->([u],[v])
unZip lista = ([fst e|e<-lista],[snd e|e<-lista])

contador :: Eq a => a -> [a] -> Int
contador item lista
    | length lista == 0 = 0
    | item /= head lista = 0
    | otherwise = 1 + contador item (tail lista)

--Função que retorna a quantidade de aparições de um elemento em uma lista de forma ordenada
organizador :: Ord a => [a] -> [(a, Int)]
organizador lista = quickSortPoli (organizadorAUX lista (head lista) 1)

organizadorAUX :: Eq a => [a] -> a -> Int -> [(a, Int)]
organizadorAUX lista anterior aux
    | length lista == 0 = []
    | aux == 1 = [(head lista, contador (head lista) lista)] ++ organizadorAUX (tail lista) (head lista) 0
    | head lista == anterior = organizadorAUX (tail lista) (head lista) 0
    | otherwise = [(head lista, contador (head lista) lista)] ++ organizadorAUX (tail lista) (head lista) 0

quickSortPoli :: Ord a => [a] -> [a]
quickSortPoli [] = []
quickSortPoli (x:xs) =
    let smallerSorted = quickSortPoli [a | a <- xs, a <= x]
        biggerSorted = quickSortPoli [a | a <- xs, a > x]
    in smallerSorted ++ [x] ++ biggerSorted

--Função que conta sequência de caractres em uma string
contarSequencia :: String -> String
contarSequencia [] = []
contarSequencia (cabeca : cauda) = auxContarSequencia 0 cabeca (cabeca : cauda)

auxContarSequencia :: Int -> Char -> String -> String
auxContarSequencia qtd letra [] = letra : show qtd
auxContarSequencia qtd letra (cabeca : cauda)
    | letra == cabeca = auxContarSequencia (qtd+1) letra cauda
    | otherwise = letra : show qtd ++ auxContarSequencia 1 cabeca cauda

--Função que comprime uma sequência de caractres em uma string
comprimir :: String -> String
comprimir "" = ""
comprimir (x : xs)
   | xs == [] = [x]
   | x /= head xs = x : (comprimir xs)
   | otherwise = x : (show cont) ++ comprimir (drop cont (x : xs))
   where
    cont = contaIguaisInicio x (x : xs)

contaIguaisInicio :: Char -> String -> Int
contaIguaisInicio _ [] = 0
contaIguaisInicio c (x : xs)
   | c == x = 1 + contaIguaisInicio c xs
   | otherwise = 0

--Função polimórfica que conta uma sequencia de elementos repetidos e retorna ordenados
contaIguaisInicioPolimorfica :: Eq t => t -> [t] -> Int
contaIguaisInicioPolimorfica _ [] = 0
contaIguaisInicioPolimorfica c (x : xs)
  | c == x = 1 + contaIguaisInicioPolimorfica c xs
  | otherwise = 0

tuplasRepeticao :: Eq t => [t] -> [(t, Int)]
tuplasRepeticao [] = []
tuplasRepeticao (x : xs)
  | xs == [] = [(x, 1)]
  | x /= head xs = (x, 1) : (tuplasRepeticao xs)
  | otherwise = (x, cont) : (tuplasRepeticao (drop cont (x : xs)))
  where
    cont = contaIguaisInicioPolimorfica x (x : xs)

filtroTuplas :: Eq t => t -> [(t, Int)] -> [(t, Int)]
filtroTuplas elem lista = [(e, n) | (e, n) <- lista, e == elem]

ordemAparicao :: Eq t => [t] -> [t]
ordemAparicao [] = []
ordemAparicao (x : xs) = [x] ++ ordemAparicao [e|e<-xs,e/=x]    

resultado::Eq t=>[t]->[(t,Int)]
resultado lista = concat [filtroTuplas ele tuplas|ele<-(ordemAparicao lista)]
  where
    tuplas =  tuplasRepeticao lista

--Funnção que comprime elementos repetidos
comprimirElem :: (Eq t, Show t) => [t] -> String
comprimirElem [] = []
comprimirElem (x : xs)
   | xs == [] = show x
   | x /= head xs = (show x) ++ (comprimirElem xs)
   | otherwise = (show x) ++ (show cont) ++ comprimirElem (drop cont (x : xs))
   where
    cont = contaIguaisInicioPolimorfica x (x:xs)


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


import System.IO
import Data.Char 
import Data.String

-- Definindo o tipo de dados para o registro
data Pessoa = Pessoa
  { nome :: String,
  idade :: Int,
  rua :: String,
  casa :: Int,
  cidade :: String
  } deriving (Eq, Show, Ord)

-- Definindo a base de dados como uma lista de pessoas
baseDados :: [Pessoa]
baseDados =
  [ Pessoa "Joao" 25 "Rua A" 20 "Caxias",
  Pessoa "Maria" 30 "Rua B" 21 "Timon",
  Pessoa "Carlos" 40 "Rua C" 21 "Caxias"
  ]

-- Função que mostra o menu e retorna a opção escolhida
mostrarMenu :: IO ()
mostrarMenu = do
  putStrLn "===== MENU ====="
  putStrLn "1. BUSCA PESSOA"
  putStrLn "2. CADASTRAR PESSOA"
  putStrLn "3. ATUALIZAR DADOS PESSOA"
  putStrLn "4. RELATÓRIO"
  putStrLn "5. IDADE MÉDIA POPULAÇÃO"
  putStrLn "6. VISUALIZAR DADOS"
  putStrLn "7. SAIR"
  putStrLn "================="
  putStrLn "Escolha uma opção: "
  opcao <- getChar
  putStrLn ""
  executarOpcao opcao
  mostrarMenu

-- Função que executa a opção escolhida pelo usuário
executarOpcao :: Char -> IO ()
executarOpcao '1' = dadosBuscaPessoa
executarOpcao '2' = cadastrarPessoa
executarOpcao '3' = putStrLn "Você escolheu a opção 3."
executarOpcao '4' = dadosBuscaCidade
executarOpcao '5' = calculaMediaIdade
executarOpcao '6' = print baseDadosOrdenada
executarOpcao '7' = putStrLn "Saindo..."
executarOpcao _ = putStrLn "Opção inválida."

-- Função para buscar uma pessoa pelo nome na base de dados
buscarPessoa :: String -> [Pessoa] -> Maybe Pessoa
buscarPessoa _ [] = Nothing
buscarPessoa name (pessoa : pessoas)
  | name == (nome pessoa) = Just pessoa
  | otherwise = buscarPessoa name pessoas

dadosBuscaPessoa :: IO ()
dadosBuscaPessoa = do
  putStrLn "Digite o nome da pessoa que você deseja buscar:"
  nome <- getLine
  let pessoaEncontrada = buscarPessoa nome baseDados
  case pessoaEncontrada of
    Just pessoa -> putStrLn $ "Pessoa encontrada: " ++ show pessoa
    Nothing -> putStrLn "Pessoa não encontrada."


-- Função para calcular a média de idade das pessoas cadastradas na base de dados
auxCalcularMediaIdade :: [Pessoa] -> Float
auxCalcularMediaIdade registros =
  let totalIdades = fromIntegral $ sum $ map idade baseDados
      quantidadeRegistros = fromIntegral $ length baseDados
  in totalIdades / quantidadeRegistros

calculaMediaIdade :: IO ()
calculaMediaIdade = do
  let mediaIdade = auxCalcularMediaIdade baseDados
  putStrLn $ "A média de idade é: " ++ show mediaIdade


-- Função para buscar na base de dados a quantidade de pessoas morando em uma cidade
buscarCidade :: String -> [Pessoa] -> Int
buscarCidade _ [] = 0
buscarCidade name (pessoa : pessoas)
  | name == (cidade pessoa) = 1+ buscarCidade name pessoas
  | otherwise = buscarCidade name pessoas

dadosBuscaCidade :: IO ()
dadosBuscaCidade = do
  putStrLn "Digite a cidade que deseja gerar relatório:"
  nome <- getLine
  let cidadeEncontrada = buscarCidade nome baseDados
  case cidadeEncontrada of
    cidade -> putStrLn $ "Pessoas morando nesta cidade: " ++ show cidade

--Função para cadastrar uma pessoa na base de dados
adicionarPessoa :: Pessoa -> [Pessoa] -> [Pessoa]
adicionarPessoa pessoa base = pessoa : base

cadastrarPessoa :: IO ()
cadastrarPessoa = do
  putStrLn "Digite o nome da Pessoa:"
  nome <- getLine
  putStrLn "Digite a Idade:"
  idade <- getLine
  putStrLn "Digite o nome da rua:"
  rua <- getLine
  putStrLn "Digite o número da casa:"
  casa <- getLine
  putStrLn "Digite o nome da cidade:"
  cidade <- getLine
  let novaPessoa = Pessoa nome (read idade :: Int) rua (read casa :: Int) cidade
  let novaBaseDados = adicionarPessoa novaPessoa baseDados
  putStrLn "Pessoa cadastrada com Sucesso !!"
  print novaBaseDados


--Função que ordena uma lista de inteiros
compararPorNome :: Pessoa -> Pessoa -> Ordering
compararPorNome p1 p2 = compare (nome p1) (nome p2)

quicksort :: (a -> a -> Ordering) -> [a] -> [a]
quicksort _ [] = []
quicksort cmp (x:xs) =
  let menores = quicksort cmp [a | a <- xs, cmp a x == LT]
      maiores = quicksort cmp [a | a <- xs, cmp a x /= LT]
  in menores ++ [x] ++ maiores

baseDadosOrdenada :: [Pessoa]
baseDadosOrdenada = quicksort compararPorNome baseDados
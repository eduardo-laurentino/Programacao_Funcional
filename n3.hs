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
  } deriving (Eq, Show)

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
executarOpcao '6' = putStrLn "Saindo..."
executarOpcao _   = putStrLn "Opção inválida."

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
cadastrarPessoa :: IO ()
cadastrarPessoa = do
    putStrLn "Digite um nome:"
    s1 <- getLine
    putStr "O nome informado foi: "
    let n1 = read s1 :: String
    putStrLn (n1)

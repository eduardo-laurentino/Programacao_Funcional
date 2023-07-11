import System.IO
import Data.Char 
import Data.String


-- Definindo o tipo de dados para o registro
data Registro = Pessoa
  { nome :: String,
  idade :: Int,
  rua :: String,
  casa :: Int,
  cidade :: String
  } deriving (Eq, Show, Ord)

imprimir :: [Registro] -> IO ()
imprimir [] = do putStrLn "\nBASE DE DADOS VAZIA"
imprimir lista = do
  putStrLn "\n--------BASE DE DADOS--------"
  print lista

-- Função para buscar na base de dados a quantidade de pessoas morando em uma cidade
buscarCidade :: String -> [Registro] -> Int
buscarCidade _ [] = 0
buscarCidade name (pessoa : pessoas)
  | name == (cidade pessoa) = 1+ buscarCidade name pessoas
  | otherwise = buscarCidade name pessoas

dadosBuscaCidade ::[Registro] -> IO ()
dadosBuscaCidade baseDados = do
  putStrLn "Digite a cidade que deseja gerar relatório:"
  nome <- getLine
  let cidadeEncontrada = buscarCidade nome baseDados
  case cidadeEncontrada of
    cidade -> putStrLn $ "Pessoas morando nesta cidade: " ++ show cidade

-- Função para calcular a média de idade das pessoas cadastradas na base de dados
auxCalcularMediaIdade :: [Registro] -> Float
auxCalcularMediaIdade registros =
  let totalIdades = fromIntegral $ sum $ map idade registros
      quantidadeRegistros = fromIntegral $ length registros
  in totalIdades / quantidadeRegistros

calculaMediaIdade :: [Registro] -> IO ()
calculaMediaIdade baseDados = do
  let mediaIdade = auxCalcularMediaIdade baseDados
  putStrLn $ "A média de idade é: " ++ show mediaIdade

-- Função que ordena a base de dados
compararPorNome :: Registro -> Registro -> Ordering
compararPorNome p1 p2 = compare (nome p1) (nome p2)

quicksort :: (a -> a -> Ordering) -> [a] -> [a]
quicksort _ [] = []
quicksort cmp (x:xs) =
  let menores = quicksort cmp [a | a <- xs, cmp a x == LT]
      maiores = quicksort cmp [a | a <- xs, cmp a x /= LT]
  in menores ++ [x] ++ maiores

baseDadosOrdenada :: [Registro] -> [Registro]
baseDadosOrdenada pessoa = quicksort compararPorNome pessoa

-- Função para buscar na base de dados a quantidade de pessoas morando em uma cidade
buscarPessoa :: String -> [Registro] -> IO ()
buscarPessoa _ [] = putStrLn "Base de dados vazia"
buscarPessoa name (pessoa : pessoas)
  | name == (nome pessoa) = putStrLn "Pessoa cadastrada!"
  | pessoas == [] = putStrLn "Pessoa não cadastrada!"
  | otherwise = buscarPessoa name pessoas

-- Função para adicionar uma pessoa na base de dados
adicionarPessoa :: [Registro] -> IO [Registro]
adicionarPessoa dados = do
  putStrLn "\n-------ADICIONAR-------"
  putStrLn "Digite o nome: "
  nome <- getLine
  putStrLn "Digite a idade: "
  idade <- getLine
  putStrLn "Digite o nome da rua: "
  rua <- getLine
  putStrLn "Digite o número da casa: "
  casa <- getLine
  putStrLn "Digite a cidade"
  cidade <- getLine
  let novaPessoa = Pessoa nome (read idade :: Int) rua (read casa :: Int) cidade
  return (novaPessoa:dados)

menu :: [Registro] -> IO ()
menu dados = do
  putStrLn "\n--------MENU--------"
  putStrLn "Digite 1 para inserir uma pessoa"
  putStrLn "Digite 2 imprimir"
  putStrLn "Digite 3 para gerar relatório"
  putStrLn "Digite 4 para calcular a média de iddade"
  putStrLn "Digite 5 para atualizar um registro da base de dados"
  putStrLn "Digite 0 para sair"
  putStr "Opção: "
  opt <- getChar
  getChar
  case opt of
    '1' -> do
      db <- adicionarPessoa dados
      putStrLn "Pessoa Adicionada"
      menu db
    '2' -> do
      imprimir (baseDadosOrdenada dados)
      menu dados
    '3' -> do
      dadosBuscaCidade dados
      menu dados
    '4' -> do
      calculaMediaIdade dados
      menu dados
    '5' -> do
      putStrLn "Digite o nome da pessoa:"
      nome <- getLine
      buscarPessoa nome dados
    '0' -> do
      putStrLn "\n--------FIM--------"
    _ -> do
      putStrLn "\nOpção inválida!"
      menu dados

main :: IO ()
main = do
  menu []
  return ()
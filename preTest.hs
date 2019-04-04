{-begin
        of this 
                tutorial-} 
import Data.List
import System.IO

-- Haskell uses Datatype inference
-- and also static type


-- Int 

maxInt = maxBound :: Int

-- Integer   is as big as we want
num = 4324627934786324732469323294823432

-- float / single precisiun . number
myFloat = 1.1

-- Double / precisium 11 points
bigdouble = 2.88888888888 

-- bool true or false

-- char

always5 :: Int
always5 = 5
 
--lists

numbers = [3,7,54,4,32,2]


-- concatenate lists

newlist = numbers ++ [45,423,1,11]


-- lists inside of lists

multlist = [[9,3,32],[21,25]]


isListEmpty = length multlist == 0

--infinty list 
list = [10,20..]

--list comprehension

-- SLIDE 2

{-Defina a funçãoo addEspacos que produz um string com uma
quantidade n de espaços-}

addEspacos :: Int -> String
addEspacos x
    | x == 0 = ""
    | x > 0 = " " ++ addEspacos (x-1)


{-Defina a função paraDireita utilizando a definição de
addEspacos para adiciconar uma quantidade n de espaços à
esquerda de um dado String, movendo o mesmo para a direita.-}

paraDireita :: Int -> String -> String 
paraDireita x str
    | x == 0 = "" ++ str
    | x > 0  = "" ++ addEspacos (x-1) ++ str


{-Escreva uma função para retornar, em forma de tabela, todas as
vendas da semana 0 até a semana n, incluindo o total e a média de
vendas no período. Usem as funções definidas previamente e
defina novas funções que achar necessário.-}

-- Enunciado confuso
imprimeSemanas :: Int -> Int
imprimeSemanas x 
    | x == 0 = 12
    | x == 1 = 14
    | x == 2 = 15 
    | x > 2  = 0 + imprimeSemanas (x-1)

{-Defina a função menorMaior que recebe três inteiros e retorna
uma tupla com o menor e o maior deles, respectivamente.-}

menorMaior :: Int -> Int -> Int -> (Int, Int)
menorMaior x y z = (menor x y z, maior x y z)

menor :: Int -> Int -> Int -> Int
menor x y z 
    | x < y && x < z = x
    | y < x && y < z = y
    | otherwise = z

maior :: Int -> Int -> Int -> Int
maior x y z 
    | x > y && x > z = x
    | y > x && y > z = y
    | otherwise = z

{-Defina a função ordenaTripla que recebe uma tripla de inteiros
e ordena a mesma.-}

ordenaTripla :: Int -> Int -> Int -> (Int, Int, Int)
ordenaTripla x y z 
    | x > y && y > z = (z, y, x)
    | y > x = ordenaTripla y x z
    | z > y = ordenaTripla x z y

{-Defina funções que retornem
• a primeira coordenada de um ponto 
• a segunda coordenada de um ponto-}

type Ponto = (Float, Float)
type Reta = (Ponto, Ponto)

fstCoord :: Ponto -> Float
fstCoord p = fst p

sndCoord :: Ponto -> Float
sndCoord p = snd p

-- indique se uma reta é vertical ou não ( x1 = x2)

retaEhVertical :: Reta -> Bool
retaEhVertical r
    | fst (fst r) == fst (snd r) = True
    | otherwise                  = False

p1 :: Ponto
p1 = (2.3,1.6)

p2 :: Ponto
p2 = (2.33,4.9)

r :: Reta
r = (p1,p2)

bool1 = retaEhVertical r

-- SLIDE 3

-- dobrar os elementos de uma lista
double :: [Int] -> [Int]
double [] = []
double (x:xs) = 2 * x : double xs

-- determinar se um valor faz parte de uma lista
member :: [Int] -> Int -> Bool
member [] a = False
member (x:xs) a 
    | x == a = True
    | otherwise = member xs a 

-- filtrar apenas os números de uma lista
digits :: String -> String
digits [] = []
digits (x:xs)
    | memberStr "0123456789" x == True = "" ++ x : digits xs
    | otherwise = digits xs 

memberStr :: String -> Char -> Bool
memberStr [] a = False
memberStr (x:xs) a 
    | x == a = True
    | otherwise = memberStr xs a 

--somar uma lista de pares
    
sumPairs :: [( Int, Int)] -> [Int]
sumPairs [] = []
sumPairs (x:xs) = fst x + snd x : sumPairs xs

                                            -- Funções sobre a base de dados - consultas

type Pessoa = String
type Livro = String
type BancoDados = [(Pessoa,Livro)]

baseExemplo :: BancoDados
baseExemplo = [("Sergio","O Senhor dos Aneis"),("Andre","Duna"),("Fernando","Jonathan S trange & Mr.Norrell"),("Fernando","Duna")]

--livros que uma determinada pessoa tem no momento
livros :: BancoDados -> Pessoa -> [Livro]
livros [] _ = []
livros (a:as) b 
    | fst a == b = snd a : livros as b
    | otherwise = livros as b

-- pessoas q determinado livro esta emprestado
emprestimos :: BancoDados -> Livro ->[Pessoa]
emprestimos [] _ = []
emprestimos (a:as) b 
    | snd a == b = fst a : emprestimos as b
    | otherwise = emprestimos as b

-- Esse livro ta emprestado ou não
emprestado :: BancoDados -> Livro -> Bool
emprestado [] _ = False
emprestado (a:as) b 
    | snd a == b = True
    | otherwise = emprestado as b

-- quantos livros determinada pessoa tem com ela
qtdEmprestimos :: BancoDados -> Pessoa -> Int
qtdEmprestimos [] _ = 0
qtdEmprestimos (a:as) b 
    | fst a == b = 1 + qtdEmprestimos as b
    | otherwise = qtdEmprestimos as b

-- add emprestimo
emprestar :: BancoDados -> Pessoa -> Livro -> BancoDados
emprestar [] p l = (p,l):[]
emprestar x p l = (p,l):x

-- remover emprestimo
devolver :: BancoDados -> Pessoa -> Livro -> BancoDados
devolver [] _ _ = []
devolver (a:as) p l 
    |fst a == p && snd a == l =  devolver as p l 
    |otherwise                = a : devolver as p l

-- mesmas funçoes mas agr com compreensao de lista
--membroCL :: [Int] -> Int -> Bool
--membroCL a x = [True|b<-a, x==b]
--livrosCL :: BancoDados -> Pessoa -> [Livro]
--emprestimosCL :: BancoDados -> Livro -> [Pessoa]
--emprestadoCL :: BancoDados -> Livro -> Bool
--qtdEmprestimosCL :: BancoDados -> Pessoa -> Int
--emprestarCL :: BancoDados -> Pessoa -> Livro -> BancoDados
--devolverCL :: BancoDados -> Pessoa -> Livro -> BancoDados

-- SLIDE 4

--função que devolve uma lista como os n primeiros elementos da lista de entrada
take1 :: [t] -> Int -> [t]
take1 [] _ = []
take1 (a:as) x 
    | x > 0 = a : take1 as (x-1)
    | x  == 0 = []

-- funcao que devolve uma lista contendo os elementos da lista de entrada, exceto pelos n primeiros
drop1 :: [t] -> Int -> [t]
drop1 [] _ = []
drop1 (a:as) x 
    | x > 0 = drop1 as (x-1)
    | otherwise = a:as

{-recebe uma função predicado e devolve uma lista
contendo todos os elementos da lista de entrada que
antecedem o primeiro para o qual a função predicado produz valor False.-}
takeWhile1 :: (a -> Bool) -> [a] -> [a]
takeWhile1 _ [] = []
takeWhile1 f (a:as)
    | f a == True = a : takeWhile1 f as
    | f a == False = []

-- msm coisa só que drop
dropWhile1 :: (a -> Bool) -> [a] -> [a]
dropWhile1 _ [] = []
dropWhile1 f (a:as)
    | f a = dropWhile1 f as
    | otherwise = a:as

-- SLIDE GENERALIZAÇAO 

--Dada uma função, verificar se ela é crescente em um intervalo de 0 a n
isCrescent :: (Int -> Int ) -> Int -> Bool
isCrescent f n 
    | n > 0 && f n > f (n-1) = True
    | otherwise = False

f1 :: Int -> Int
f1 n = 7*n+1

sumList :: [Int] -> Int
sumList l = foldr1 (+) l

        --Defina as seguintes funções sobre listas

--eleva os itens ao quadrado (mapping)
mySqrt :: [Int] -> [Int]
mySqrt [] = []
mySqrt (a:as) = a*a : mySqrt as

--retorna a soma dos quadrados dos itens (folding)
sumSqrt :: [Int] -> Int
sumSqrt [] = 0
sumSqrt (a:as) = a*a + sumSqrt as

--manter na lista todos os itens maiores que zero (filtering)
myFilter :: [Int] -> [Int]
myFilter [] = []
myFilter (a:as)
    | a>0 = a : myFilter as
    | otherwise = myFilter as
    

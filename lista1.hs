-- Resolução dos exercícios da lista 1 - plc

{-1°) A expressao "map.(.)" está incorreta pois a função "." necessita de parametros para ser utilizada e
por isso não pode ser passada como um parâmetro para a mesma função "."-}

{-2°) Função que retorna todas as listas de uma lista dada como argumento -}

sublistas2 :: [a] -> [[a]]
sublistas2 []      =  [[]]
sublistas2 (x:xs)  =  [ x:ys | ys <- sublistas2 xs ] ++ sublistas2 xs

--3°
--a) 
poli :: Int -> Int -> Int -> Int -> Int
poli a b c = (\x -> a*(x*x) + b*x + c )

{-b) Defina a funcao listaPoli :: [( Integer,Integer,Integer)] −> [Integer−>Integer] que aguarda
uma lista de triplas de inteiros (coeficientes de um polinomio de segundo grau) e devolve uma
lista de func̃oes de inteiro para inteiro (polinomios) .-}

listaPoli :: [(Int,Int,Int)] -> [Int -> Int]
listaPoli l = [poli a b c | (a,b,c) <- l]

{-c) Defina a funcao appListaPoli :: [Integer−>Integer] −> [Integer] −> [Integer] que recebe
uma lista de funcoes de polinomios e uma lista de inteiros. Esta func̃ao devolve uma lista
de inteiros que resultam da aplicacao de cada polinomio da primeira lista aplicada ao inteiro
correspondente na segunda lista.-}

appListaPoli :: [Int->Int] -> [Int] -> [Int]
appListaPoli [] _  = []
appListaPoli _ []  = []
appListaPoli (f:fs) (x:xs) = f x : appListaPoli fs xs

{-USANDO COMPREENSAO DE LISTA appListaPoli :: [Int -> Int] -> [Int] -> [Int]
appListaPoli lp lint = [p i | p <- lp, i <- lint]-}

--4°
--a)
isMatriz :: [[t]] -> Bool
isMatriz [a] = True
isMatriz (a:b:ls)
    | length a == length b = isMatriz (b:ls)
    | otherwise = False

--b) permutar a posição de duas linhas (take,drop,init e !!)           [[1,2,3],[4,5,6],[7,8,9],[10,11,12],[44,78]]
{-permute :: [[t]] -> Int -> Int -> [[t]]
permute [] = []
permute l@(a:as) x y 
    | x < y = init (take ls x) :  NAO CONSEGUI RESOLVER
    |
        where 
            lx = getLine ls x
            ly = getLine ls y

permute2 :: [[t]] -> Int -> Int -> [[t]] -- [[1,2,3],[4,5,6],[7,8,9],[10,11,12],[44,78]]
permute2 [] = []
permute2 l@(a:as) x y taml
    |   tiver em x   = init l : (!!) 
    |
    |otherwise = a : permute l
    where taml = length l

getLine1 :: [[t]] -> Int -> [t]
getLine1 a n = (!!) a n -}

{-5° Implemente a funcao filtrarEInserir :: [[Int]] -> Int -> ([[Int]], Int) que retorna uma
tupla. O primeiro elemento da tupla  ́e constituıdo de listas de inteiros tais que a soma dos numeros
ımpares ́e maior que a soma dos numeros pares. O segundo elemento consiste no produto entre o
segundo argumento da funcao filtrarEInserir e a multiplicacao da maior soma obtida das listas
retornadas. Utilize obrigatoriamente filter.-}

filtrarEInserir :: [[Int]] -> Int -> ([[Int]], Int)
filtrarEInserir [[]] _ = ([],0)
filtrarEInserir  l x = (filter (sumImparMaior) l, (mult (maiorSoma (filter (sumImparMaior) l)) * x ))

sumImparMaior :: [Int] -> Bool
sumImparMaior l = sumImpar l > sumPar l

sumImpar :: [Int] -> Int
sumImpar [] = 0
sumImpar (a:as)
    | (a `mod` 2 /= 0) = a + sumImpar as
    |  otherwise = sumImpar as

sumPar :: [Int] -> Int
sumPar [] = 0
sumPar (a:as)
    | (a `mod` 2 == 0) = a + sumPar as
    |  otherwise = sumPar as

maiorSoma :: [[Int]] -> [Int]
maiorSoma [x] = x
--maiorSoma [[z]] = [z]
maiorSoma (a:b:ls) 
    | sum a > sum b = a
    | otherwise = maiorSoma (b:ls)

mult :: [Int] -> Int
mult [x] = x
mult (a:as) = a * mult as

--6° 
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f1 f2 l = combineLists (func1 f1 l) (func2 f2 l) 

func1 :: (a -> b) -> [a] -> [b]
func1 _ [] = []
func1 f [x] = f x : []
func1 f (a:b:as) = f a : func1 f as 

func2 :: (a -> b) -> [a] -> [b]
func2 _ [] = []
func2 _ [x] = []
func2 f (a:b:as) = f b : func2 f as


combineLists :: [a] -> [a] -> [a]
combineLists x [] = x
combineLists [] y = y
combineLists (x:xs) (y:ys) = [x,y] ++ combineLists xs ys

--7°
--a)
type Codigo = Int
type Urna = [Voto]
type Apuracao = [(Voto, Int)]

data Voto = Presidente Codigo | Senador Codigo | Deputado Codigo| Branco deriving (Show)

instance Eq Voto where 
    Presidente p1 == Presidente p2 = p1 == p2
    Senador s1 == Senador s2 = (s1 == s2)
    Deputado d1 == Deputado d2 = (d1 == d2)
    Branco == Branco = True
    _ == _ = False

voto1 = Presidente 1
voto2 = Presidente 2

--b)
totalVotos :: Urna -> Voto -> Int
totalVotos [] v = 0
totalVotos (a:as) v
    | a == v = 1 + totalVotos as v
    | otherwise =  totalVotos as v


--c)
apurar :: Urna -> Apuracao 
apurar [] = []
apurar (x:xs) = (x, 1 + totalVotos xs x): apurar (filter (/=x) xs) 







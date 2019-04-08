-- Resolução dos exercícios da lista 2 - plc

{-1º) Escreva uma funcao f :: [Int] −> [Int] que retorna uma lista contendo todos os
elementos da lista dada como argumento que ocorrem duas vezes em sucessao.-}

--a)
f :: [Int] -> [Int]
f  [] = []
f [x] = []
f (a:b:ls)
    | a == b = a : f (b:ls)
    | otherwise = f (b:ls)

--b) mesma coisa mas com compreensao de lista

fCL (l:ls) = [x | (x,y) <- (zip (l:ls) ls), x == y]

{-2º) Defina uma fuņc̃ao g ::[ Int] −> Bool que verifica que todo elemento de uma lista
que est ́a entre 0 e 100 (inclusive)  ́e par. Utilize as fun ̧c oes map, filter e foldr.-}

g :: [Int] -> Bool
g [] = True
g l = foldr (==) True (map even (filter (<= 100) (filter (>=0) l)))

{-Uma lampada  ́e caracterizada por ser compacta ou incandescente. Alem, disso, toda
lampada possui o nome do seu fabricante e a potencia como um valor em Watts.

(a) Defina o tipo algebrico Lampada, de acordo com as caracterısticas descritas-}

type Fabricante = String
type Potencia = Int

data Lampada =  Incandescente Fabricante Potencia | Compacta Fabricante Potencia 

{-(b) Estabeleca que exibir uma lampada resulta em uma string que comeca com a
palavra “Compacta”, no caso de lampada compacta, ou com a string “Incan-
descente”. Estas strings sao seguidas do nome do fabricante e da potencia da
lampada. Ou seja, defina que o tipo Lampada  ́e instancia da classe Show.-}

instance Show Lampada where 
    show (Incandescente f1 p1) = ("Incandescente "++ f1 ++ " "++ show p1 ++ " Watts") 
    show (Compacta f1 p1) = ("Compacta "++ f1 ++ " "++ show  p1 ++ " Watts") 

{-(c) Estabeleca que o tipo Lampada  ́e uma instancia da classe Eq, de modo que
duas lampadas sao iguais se forem compactas e possuırem o mesmo fabricante
e potencia. O mesmo vale para lampadas incandescentes.-}

instance Eq Lampada where 
    (Incandescente f1 p1) == (Incandescente f2 p2) = f1 == f2 && p1==p2
    (Compacta f1 p1) == (Compacta f2 p2) = f1 == f2 && p1==p2
    _ == _ = false




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
g (a:as)
    | a `mod` 2 == 0 || a > 100 = g as
    | otherwise = False






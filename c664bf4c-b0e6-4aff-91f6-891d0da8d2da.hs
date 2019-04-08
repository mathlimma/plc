-- Resolução dos exercícios da lista 1 - plc

{-1°) A expressao "map.(.)" está incorreta pois a função "." necessita de parametros para ser utilizada e
por isso não pode ser passada como um parâmetro para a mesma função "."-}

{-2°) Função que retorna todas as listas de uma lista dada como argumento -}

sublistas :: [a] -> [[a]]
sublistas [] = []
sublistas (a:as) = [a] : sublistas as ++ [a] ++ sublistas as
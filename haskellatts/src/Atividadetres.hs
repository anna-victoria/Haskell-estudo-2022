module Atividadetres where

-- questao um
somaate100quadrado = sum [x * x | x <- [1 .. 100]]

-- questao dois

grid :: Int -> Int -> [(Int,Int)]
grid m n = [(x,y) | x <- [0 .. m], y <- [0 .. n]]


-- off topic
replicar x z = [x | _ <- [1 .. z]]

-- questao tres
quadrado :: Int -> [(Int,Int)]
quadrado n = [ (x,y) | (x,y) <- grid n n, x /= y]

-- questao quatro
meureplicate :: Int -> a -> [a]
meureplicate n p = [p | _ <- [1..n]]

-- questao cinco
pitag :: Int -> [(Int, Int, Int)] 
--pitag n = [(x,y,z) | _ <- [1 .. n], x * x + y * y == (z * z)]
pitag n = [(x,y,z) | x <- [3 .. n], y <- [3 .. n], z <- [3 .. n], (x * x) + (y * y) == (z * z)]

-- questao seis
-- Um inteiro positivo é perfeito se ele é igual à soma de todos os seus fatores,
-- excluindo o próprio número. Usando compreensão de listas e a função fatores, defina 
--a função perfeitos :: Int -> Int que retorna
-- a lista de todos os números perfeitos menores que um limite informado como argumento.

fatores :: Int -> [Int]
fatores n = [x | x <- [1 .. n], mod n x == 0]

perfeitos :: Int -> [Int]
--perfeitos n = [x | x <- [] , guarda]
perfeitos n = [x | x <- [1 .. n] , x == sum (init (fatores x))] 
-- init pega a lista de todos os elementos menos o ultimo elemento

--questao sete
-- Mostre que a compreensão de lista [(x,y) | x <- [1,2], y <- [3,4]], com dois geradores, pode ser 
--representada usando duas compreensões de lista, cada uma com apenas um gerador. 
--Dica: Procure usar a função concat. 

--concat [[(1,3),(1,4)] [(2,3), (2,4)]]
concatenarfun = concat  [[(1,y) | y <- [3,4]], [(2,y) | y <- [3,4]]]

--questão oito
-- 
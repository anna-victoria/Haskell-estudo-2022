module Atividadetres where

-- questao um
somaate100quadrado :: Integer
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
concatenarfun :: [(Integer, Integer)]
concatenarfun = concat  [[(1,y) | y <- [3,4]], [(2,y) | y <- [3,4]]]

--questão oito
-- Redefina a função posicoes usando a função buscar, disponível em 
-- https://emanoelbarreiros.github.io/funcional/haskell-5#a-fun%C3%A7%C3%A3o-zip


buscar :: Eq a => a -> [(a,b)] -> [b]
buscar k xs = [v | (k', v) <- xs, k == k']


posicoes :: Eq a => a -> [a] -> [Int]
posicoes x xs = [i | (x', i) <- zip xs [0 ..], x == x']

-- *Questaooito> posicoes 3 [1,50,5,85,3,8,89]
-- [4]
-- *Questaooito> posicoes 3 [1,50,5,85,3,8,89,3,7,7,5,3]
-- [4,7,11]

-- zip xs [0 ..] retorna uma lista de tuplas (x', i) onde x' é o elemento de xs e i é o índice de xs
-- zip pega duas listas e combina-as em uma lista de tuplas
-- avaliação preguiçosa é o processamento incompleto de uma função, onde o haskell determina a parada
-- da função por indução do contexto. Indução do contexto é o ato do haskell ver a função por completo
-- e entender que uma parte da função se esgota antes da outra, seja por condicional ou por variaveis.

meuposicoes :: (Eq a, Num b, Enum b) => a -> [a] -> [b]
meuposicoes x xs = buscar x (zip xs [0 ..]) 
-- jeito mais simples de fazer o meuposicoes

meuposicoes' :: (Eq a, Num b, Enum b) => a -> [a] -> [b]
meuposicoes' x xs = buscar x [(a, b) | (a, b) <- zip xs [0 ..]]
-- outra forma usando compreensão de listas

--questao nove
-- O produto escalar de duas listas de inteiros xs e ys de tamanho n é dado pelo produto dos inteiros em posições correspondentes:
-- n-1
--  E     (xs i * ys i)
-- i = 0
-- imagem na pasta

produtoescalar :: [Int] -> [Int] -> Int
produtoescalar [] _ = 0
produtoescalar _ [] = 0
produtoescalar (x:xs) (y:ys) = x * y + produtoescalar xs ys


produtoescalar' :: Num a => [a] -> [a] -> a
produtoescalar' x y = sum [a * b | (a, b) <- zip x y]
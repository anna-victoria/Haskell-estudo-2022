module AttDois where

-- questão um
-- Utilizando a função soma, faça uma função que calcule a multiplicação entre dois números quaisquer, 
-- considerando números positivos e negativos.

multisoma a b = somalista (replicate a b)
    where
        somalista [] = 0
        somalista (x:xs) = x + somalista xs

-- replicate é uma função que repete um elemento em uma lista

multisoma' a b = if a > 0 && b > 0 || a < 0 && b < 0
                 then res
                 else negate res
                 where 
                    res = sum (replicate(abs a) (abs b))

-- questão dois
seqRaiz6 1 = sqrt 6
seqRaiz6 n = sqrt(6 + seqRaiz6 (n-1))

seqRaiz6' n
    | n < 1 = error "n deve ser maior que 0"
    | n == 1 = sqrt 6
    | otherwise = sqrt(6 + seqRaiz6' (n-1))

-- questão três
-- Defina uma função que, dada uma lista numérica, retorne uma tupla, que contenha o maior elemento da lista bem como seu índice

maiorDaLista1 lista  =  maiorDaListaAux1 (tail novaLista) (head novaLista) 
                       where
                            novaLista = zip lista [0 .. ]
maiorDaListaAux1 [] maiorAtual1 = maiorAtual1
maiorDaListaAux1 (x:xs) maiorAtual1 = if fst x > fst maiorAtual1 
                                    then maiorDaListaAux1 xs x
                                    else maiorDaListaAux1 xs maiorAtual1

-- zip é uma função que recebe duas listas e retorna uma lista de tuplas, 
--onde cada tupla tem dois elementos, o primeiro é o elemento da primeira lista e o segundo é o elemento da segunda lista



-- questão quatro
-- Defina uma função que converta uma lista de dígitos (unitários, 0 a 9) em uma outra lista, que é a sua tradução em String.

conv [] = []
conv (n:ns) = [dic10 !! n] ++ conv ns  -- indexes are zero based, so [1,2,3] !! 0 will result in 1.   e  ++ é concatenado
              where
                dic10 = ["zero", "um", "dois", "tres", "quatro", "cinco", "seis", "sete", "oito", "nove"]

-- !! é o operador de indexação, ou seja, [1,2,3] !! 0 will result in 1.
-- (n:ns) é a lista de números, ou seja, [1,2,3]
-- ns é a lista de números restantes, ou seja, [2,3]
-- n é o primeiro número da lista, ou seja, 1

conv' [] = []
conv' (n:ns) = dic10 !! n : conv' ns -- : é concatenar também
              where
                dic10 = ["zero", "um", "dois", "tres", "quatro", "cinco", "seis", "sete", "oito", "nove"]


-- questão cinco
-- Construa uma função delPosicaoN :: [Int] -> Int -> [Int] em que dada uma lista de inteiros e a posição de 
--um elemento qualquer, retorne uma nova lista sem aquele elemento da n-ésima posição

delPosicaoN :: Int -> [a] -> [a]
delPosicaoN n l = take n l ++ drop (n+1) l

delPosicaoN' :: [Int] -> Int -> [Int]
delPosicaoN' l n = take n l ++ drop (n+1) l
 -- drop de n para frente, ou seja, drop n l remove os n primeiros elementos de l
 -- take n l retorna os n primeiros elementos de l
 -- ++ é concatenar
 -- take n l ++ drop (n+1) l retorna os n primeiros elementos de l, mas sem o n-ésimo elemento
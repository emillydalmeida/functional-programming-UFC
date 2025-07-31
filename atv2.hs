-- IDENTIFICAÇÃO
matricula = "514672" -- coloque a matricula aqui entre as asspas

-- Nome
nome = "Emilly Almeida" -- coloque seu nome aqui entre aspas

-- ATIVIDADE 2

-- Esta atuvidade visa construir uma 
-- função que determine os n primeitos números primos

-- Construa as funções a seguir,

-- determina os divisores de x excluindo o 1
divisores :: Int -> [Int]
divisores x = [d | d <- [2..x-1], x`mod`d==0]

-- Determina se um números x é ou não primo
eprimo :: Int -> Bool
eprimo x = null (divisores x)

-- cria lista com n primeiros primos
primos :: Int -> [Int]
primos n = take n [x | x <- [2..],eprimo x]
-- ATIVIDADE 
atividade = 5

-- IDENTIFICAÇÃO
matricula = "514672" -- coloque a matricula aqui entre as asspas

-- Nome
nome = "Emilly Efanny Dantas de Almeida" -- coloque seu nome aqui entre aspas

-- 1

-- FUNÇÕES HASKELL A FAZER,

-- Construa função que 
-- receba uma string e 
-- retorne a lista das 
-- tuplas das frequencias dos
-- seus caracteres

freq :: [Char] -> [(Char, Int)]
freq s = [(c, count c s) | c <- removeDuplicates s]
    where count c s = length [x | x <- s, x == c]
          removeDuplicates [] = []
          removeDuplicates (x:xs) = x : removeDuplicates (filter (/=x) xs)

-- 2

-- Construa função que ordene
-- a lista de tuplas da questão
-- por valor de frequencia,

freqSort :: [(Char, Int)] -> [(Char, Int)]
freqSort [] = []
freqSort (x:xs) = insert x (freqSort xs)
  where insert x [] = [x]
        insert (c1,f1) ((c2,f2):ys)
          | f1 <= f2  = (c1,f1) : (c2,f2) : ys
          | otherwise = (c2,f2) : insert (c1,f1) ys



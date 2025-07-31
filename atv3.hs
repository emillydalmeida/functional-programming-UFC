-- IDENTIFICAÇÃO
matricula = "514672" -- coloque a matricula aqui entre as asspas

-- Nome
nome = "Emilly Almeida" -- coloque seu nome aqui entre aspas

-- ATIVIDADE 3

-- Remove espaços existentes no início
-- e final de uma strings dada.

strip :: [Char] -> [Char]
strip xs = stripEnd (stripStart xs)
  where
    stripStart [] = []
    stripStart (' ':xs) = stripStart xs
    stripStart xs = xs
    
    stripEnd [] = []
    stripEnd xs = reverse (stripStart (reverse xs))

-- Separa a primeira palavra do restante
-- de uma string (Palavras são substeings 
-- separadas por espaços). Exemplo,

-- >> popWord "casa  de tijolos"
-- ["casa", "de tijolos"]'
-- >>

popWord :: [Char] -> ([Char], [Char])
popWord xs = (word, dropWhile (== ' ') rest)
  where
    (word, rest) = popWordHelper xs
    
    popWordHelper [] = ([], [])
    popWordHelper (' ':xs) = ([], xs)
    popWordHelper (x:xs) = let (word, rest) = popWordHelper xs
                           in (x:word, rest)


-- Processa uma string e retorna 
-- a lista de suas palavras. OBS: 
-- palavras não devem ter espaços 
-- extemos e nem serem vazias. Exemplo,

-- >> splitStr " The   fox jumped  "
-- ["The", "fox", "jumped"]


splitStr :: [Char] -> [[Char]]
splitStr xs = splitHelper (strip xs)
  where
    splitHelper [] = []
    splitHelper xs = word : splitHelper rest
      where
        (word, rest) = popWord xs
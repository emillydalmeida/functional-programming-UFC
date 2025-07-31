-- IDENTIFICAÇÃO
matricula = "514672" 

-- Nome
nome = "Emilly Almeida" 


l0 :: [a] -> [a]
l0 l = take (length l - 2) (tail l)
lSemExtremos :: [a] -> [a]
lSemExtremos l = tail (take (length l - 1) l)
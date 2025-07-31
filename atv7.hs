-- IDENTIFICAÇÃO

atividade = 7

nome = Emilly Almeida"

matricula = "514672"

-- TIPO DE DADOS

-- Representa polinômio como 
-- um vetor de seus coeficientes

-- através de seus coeficientes

data Poly = Poly [Float]

-- IMPLEMENTARS

-- Instância de Show que permite 
-- imprimir um polinômio

instance Show Poly where
    show (Poly []) = "0"
    show (Poly coeffs) = removeLeadingPlus (concat (zipWith showTerm coeffs [0..]))
      where
        showTerm :: Float -> Int -> String
        showTerm 0 _ = ""
        showTerm coeff 0 = showCoeff coeff
        showTerm coeff 1 = showCoeff coeff ++ "x"
        showTerm coeff n = showCoeff coeff ++ "x^" ++ show n

        showCoeff :: Float -> String
        showCoeff coeff | coeff < 0 = "-" ++ show (-coeff)
                        | otherwise = "+" ++ show coeff

        removeLeadingPlus :: String -> String
        removeLeadingPlus ('+':xs) = xs
        removeLeadingPlus xs = xs



-- Exemplos
-- Main> Poly [1,2,3]
-- 1.0+2.0x+3.0X^2
-- *Main> Poly [-2,1,0]
-- -2.0+1.0x
-- *Main> Poly [-1,0,-1]
-- -1.0-1.0X^2


--AVALIAÇÃO DE POLINÔMIOS

-- Avalia um poliômio P 
-- dado x, ou seja, calcula P(x) 

avalPoly :: Poly -> Float -> Float

avalPoly (Poly coeffs) x = evalCoeffs coeffs x 0
  where
    evalCoeffs :: [Float] -> Float -> Float -> Float
    evalCoeffs [] _ res = res
    evalCoeffs (c:cs) x res = evalCoeffs cs x (res * x + c)


-- Exemplos
-- *Main> avalPoly (Poly [1,2,3]) 5
-- 86.0
-- *Main> avalPoly (Poly [-1,1,3]) 5
-- 79.0
-- *Main> avalPoly (Poly [11,0,2,2]) 3
-- 83.0

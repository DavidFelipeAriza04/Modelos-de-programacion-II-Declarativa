-- Integrantes:
-- David Ariza 20221020029
-- Oscar Contreras 20221020052

main :: IO ()
main = print "Tarea 1"

-- Suma digitos de un numero
sumaDigitos :: Integer -> Integer
sumaDigitos num
  | num < 10 = num
  | otherwise = mod num 10 + sumaDigitos (div num 10)

-- Producto digitos de un numero
multDigitos :: Integer -> Integer
multDigitos num
  | num < 10 = num
  | otherwise = mod num 10 * multDigitos (div num 10)

-- Base 10 a binario
binario :: Integer -> String
binario num
  | num == 1 = "1"
  | otherwise = binario (div num 2) ++ show (mod num 2)

-- Rectas perpendiculares
perp :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer) -> String
perp (x1, y1) (x2, y2) (u1, v1) (u2, v2)
  | (x2 - x1) * (u2 - u1) + (y2 - y1) * (v2 - v1) == 0 = "Son perpendiculares"
  | otherwise = "No son perpendiculares"

-- Integrantes:
-- David Ariza 20221020029
-- Oscar Contreras 20221020052

main :: IO ()
main = print "Tarea 2"

-- Numero a String
numToString :: Integer -> String
numToString = show

-- Palindromo
invertirLista :: [a] -> [a]
invertirLista [] = []
invertirLista (x:xs) = invertirLista xs ++ [x]

esPalindromo :: String -> Bool
esPalindromo palabra = palabra == invertirLista palabra

convertirAPalindromo :: String -> String
convertirAPalindromo palabra
    | esPalindromo palabra = palabra
    | otherwise = invertirLista(tail palabra)   ++ palabra

-- Contar Palabras
contarPalabras :: String -> Int
contarPalabras [] = 0
contarPalabras text = length (words text)
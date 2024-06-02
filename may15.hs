---- Actividad 15 mayo
-- David Ariza 20221020029
-- Oscar Contreras 20221020052
import Text.Regex (matchRegex, mkRegex)

-- Base de datos para los prestamos de la Biblioteca
type Usuario = String

type Libro = String

type ID = String

type Email = String

type LibrosEmail = [(Usuario, Email, Libro, ID)]

librosEmail =
  [ ("Juan Perez", "juan@gmail.com", "Introduction to Functional Programming", "1"),
    ("Alicia Rodriguez", "alicia@gmail.com", "Introduction to Functional Programming", "1"),
    ("Juan Perez", "juan@gmail.com", "Algebra Lineal", "2"),
    ("Luis Perez", "luis@hotmail.com", "Algebra Lineal", "2")
  ]

-- Funcion que buscar personas que poseen un determinado libro solo con ingresar parte del nombre del libro.
buscarLibro :: LibrosEmail -> Libro -> [Usuario]
buscarLibro [] _ = []
buscarLibro ((usuario, email, libro, id) : prestamos) libroBuscado =
  if matchRegex (mkRegex (".*" ++ libroBuscado ++ ".*")) libro /= Nothing
    then usuario : buscarLibro prestamos libroBuscado
    else buscarLibro prestamos libroBuscado

-- Numero de libros que se le presteo a una persona
numlibrosEmail :: LibrosEmail -> Usuario -> Int
numlibrosEmail [] u = 0
numlibrosEmail ((v, _, _, _) : prestamos) u =
  if u == v
    then
      1 + (numlibrosEmail prestamos u)
    else
      numlibrosEmail prestamos u

-- Expresión regular para validar emails
emailRegex :: String
emailRegex = "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$"

-- Función para validar el email
isValidEmail :: String -> Bool
isValidEmail email = case matchRegex (mkRegex emailRegex) email of
  Just _ -> True
  Nothing -> False

-- Devolver nombre de un libro dado un id
nombreLibroEmail :: LibrosEmail -> ID -> Libro
nombreLibroEmail [] id = ""
nombreLibroEmail ((u, e, l, i) : prestamos) id =
  if i == id
    then
      l
    else
      nombreLibroEmail prestamos id

-- Función para agregar un nuevo préstamo
prestarLibroEmail :: LibrosEmail -> Usuario -> Email -> ID -> LibrosEmail
prestarLibroEmail prestamos usuario email id
  | numlibrosEmail prestamos usuario >= 2 = prestamos
  | not (isValidEmail email) = error "Email no válido"
  | otherwise = (usuario, email, nombreLibroEmail prestamos id, id) : prestamos
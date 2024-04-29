-- Base de datos para los prestamos de la Biblioteca
type Usuario = String

type Libro = String

type Prestamos = [(Usuario, Libro)]

-- Base de datos de prueba
datosprueba =
  [ ("Juan Perez", "Introduction to Functional Programming"),
    ("Alicia Rodriguez", "Introduction to Functional Programming"),
    ("Juan Perez", "Algebra Lineal"),
    ("Luis Perez", "Algebra Lineal")
  ]

-- La lista de libros que se le presto a un usuario
libros :: Prestamos -> Usuario -> [Libro]
libros prestamos u = [l | (v, l) <- prestamos, v == u]

-- La lista de Usuarios a los que se presto un determinado libro
usuarios :: Prestamos -> Libro -> [Usuario]
usuarios [] l = []
usuarios ((u, l') : prestamos) l =
  if l == l'
    then do
      u : usuarios prestamos l
    else
      usuarios prestamos l

printPrestamos :: Prestamos -> IO ()
printPrestamos [] = return ()
printPrestamos ((u, l') : prestamos) = do
  putStrLn $ "Usuario: " ++ u ++ " Libro: " ++ l'
  printPrestamos prestamos

-- Dado un libro, esta prestado?
prestado :: Prestamos -> Libro -> Bool
prestado [] l = False
prestado ((u, l') : prestamos) l = l == l' || (prestado prestamos l)

-- Numero de libros que se le presteo a una persona
numlibros :: Prestamos -> Usuario -> Int
numlibros [] u = 0
numlibros ((v, l) : prestamos) u =
  if u == v
    then
      1 + (numlibros prestamos u)
    else
      numlibros prestamos u

-- Prestamo de libros (agregamos el prestamo a la base de datos)
prestar :: Prestamos -> Usuario -> Libro -> Prestamos
prestar prestamos nuevousuario nuevolibro = (nuevousuario, nuevolibro) : prestamos

-- Devolucion de libros (sacamos ese prestamo de la base de datos)
devolver :: Prestamos -> Usuario -> Libro -> Prestamos
devolver ((u', l') : prestamos) u l =
  if u == u' && l == l'
    then
      prestamos
    else
      (u', l') : (devolver prestamos u l)

--Diccionario Nuevo llamado libro: (id, nombrelibro)
-- A la funcion de usuarios se le pasa el id del nombre en vez del nombre del libro
--Si la cantidad de libros prestados a un usuario es mayor a x, no le preste mas
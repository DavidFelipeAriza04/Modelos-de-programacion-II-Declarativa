---- Actividad 13 mayo
type Usuario = String

type Libro = String

type ID = String

type LibrosId = [(Usuario, Libro, ID)]

librosId =
  [ ("Juan Perez", "Introduction to Functional Programming", "1"),
    ("Alicia Rodriguez", "Introduction to Functional Programming", "1"),
    ("Juan Perez", "Algebra Lineal", "2"),
    ("Luis Perez", "Algebra Lineal", "2")
  ]

-- Funcion que devuelve los usuarios a los que se les ha prestado un libro con un id en especifico
usuariosId :: LibrosId -> ID -> [Usuario]
usuariosId [] id = []
usuariosId ((u, l, i) : prestamos) id =
  if i == id
    then
      u : usuariosId prestamos id
    else
      usuariosId prestamos id

-- Numero de libros que se le presteo a una persona
numlibrosID :: LibrosId -> Usuario -> Int
numlibrosID [] u = 0
numlibrosID ((v, l, i) : prestamos) u =
  if u == v
    then
      1 + (numlibrosID prestamos u)
    else
      numlibrosID prestamos u

-- Devolver nombre de un libro dado un id
nombreLibro :: LibrosId -> ID -> Libro
nombreLibro [] id = ""
nombreLibro ((u, l, i) : prestamos) id =
  if i == id
    then
      l
    else
      nombreLibro prestamos id

-- Funcion que presta un libro a un usuario si este no ha prestado mas de x libros
prestarId :: LibrosId -> Usuario -> ID -> LibrosId
prestarId prestamos u id =
  if numlibrosID prestamos u >= 2
    then
      prestamos
    else
      (u, nombreLibro prestamos id, id) : prestamos

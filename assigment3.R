# inicializa inv como NULL
# define la funcion de conjunto para un nuevo valor de la matriz en el entorno principal
# si hay una nueva matriz, restablecer inv a NULL
# define la función get que devuelve el valor del argumento de la matriz

makeCacheMatrix <- function(x = matrix(sample(1:100,9),3,3)) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}
# Esta función calcula la inversa de la "matriz" especial devuelta por la funcion makeCacheMatrix 
# Si ya se ha calculado la inversa
# luego cacheSolve recuperará el inverso del caché
cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting inversed matrix")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
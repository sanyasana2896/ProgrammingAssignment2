makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list (set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}
cacheSolve <- function(x,...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message(" getting cached matrix")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
my_matrix <- makeCacheMatrix(matrix(4:8,2,2))
my_matrix$get()
my_matrix$getInverse()
cacheSolve(my_matrix)
cacheSolve(my_matrix)
my_matrix$getInverse()
my_matrix$set(matrix(c(4,4,8,6),2,2))
my_matrix$get()
my_matrix$getInverse()
cacheSolve(my_matrix)
cacheSolve(my_matrix)
my_matrix$getInverse()

## Caching the Inverse of a Matrix:
##Matrix Inversion is a very useful computation and there are many benefits to 
##the caching the inverse of a matrix rather than computing it repeatedly.
## Below are a pair of functions that I have used to create a special object that 
##stores a matrix and caches its inverse. 
## This function creates a special "matrix" object that can cache its inverse. 

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
##The function given below computes the inverse of the special "matrix" created 
##by makeCacheMatrix above. If the inverse has already been calculated and the 
##matrix has not been changed, then it should retireve the inverse from the cache.  

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

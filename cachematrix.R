## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}





## Write a short comment describing this function

cacheSolve<- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  mat <- x$get()
  m <- solve(mat, ...)
  x$setInverse(m)
  m
}

x1= c(10,7)

x2= c(5,3)
x.all=c(x1,x2)

nam=c("Maria","mixalis") # gives names to each element of an object


Marias_Matrix=matrix(x.all, 2, 2,dimnames =list(c("Math","Physics"),nam) )

results<-makeCacheMatrix(Marias_Matrix)
results2=makeCacheMatrix(matrix(1:4, 2, 2))
cacheSolve(results)
cacheSolve(results2)

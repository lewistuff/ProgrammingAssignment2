## These functions allow for the creation of a matrix that 
## can cache its inverse, e.g.
##  > m <- matrix(c(3,1,2,1),2,2)
##  > x <- makeCacheMatrix(m)
##  > inv <- cacheSolve(x)
##  > inv

## This function creates a custom 'matrix', which 
##  - handles caching of its inverse, 
##  - Returning a list of helper functions.
makeCacheMatrix <- function(matrix = matrix()) {
  inverseMatrix <- NULL
  set <- function(y) {
    matrix <<- y
    inverseMatrix <<- NULL
  }
  get <- function() matrix
  setinverse <- function(inverse) inverseMatrix <<- inverse
  getinverse <- function() inverseMatrix
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function will return the inverse matrix, 
##  - from the cache, if available,
##  - otherwise, it will compute and cache the result.
cacheSolve <- function(cacheMatrix, ...) {
  inverseMatrix <- cacheMatrix$getinverse()
  if(!is.null(inverseMatrix)) {
    message("getting cached matrix")
    return(inverseMatrix)
  }
  matrix <- cacheMatrix$get()
  inverseMatrix <- solve(matrix, ...)
  cacheMatrix$setinverse(inverseMatrix)
  inverseMatrix  
}

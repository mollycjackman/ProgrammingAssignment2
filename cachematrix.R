##Caching the inverse of a matrix

## This function creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, 
       setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special matrix returned by makeCacheMatrix.
##If the inverse has already been calculated (and the matrix has not been changed), 
##then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x,...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  } 
  matrix <- x$get()
  i <- solve(matrix, ...)
  x$setinverse(i)
  i
}
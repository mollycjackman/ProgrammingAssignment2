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

cache<-makeCacheMatrix(x)

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
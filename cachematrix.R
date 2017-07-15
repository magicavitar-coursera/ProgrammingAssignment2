## Helper functions for computing and caching the inverse of a matrix.

## makeCacheMatrix() creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(mtrix = matrix()) {
  inverse <- NULL
  set <- function(newmtrix) {
    mtrix <<- newmtrix
    inverse <<- NULL
  }
  get <- function() mtrix
  setinverse <- function(newinverse) inverse <<- newinverse
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve() computes and caches the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(specialmatrix, ...) {
  inverse <- specialmatrix$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  mtrix <- specialmatrix$get()
  inverse <- solve(mtrix, ...)
  specialmatrix$setinverse(inverse)
  inverse
}
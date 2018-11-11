## These functions set and cache the inverse of a matrix

## This function creates a list of functions for setting and 
#retrieving a cached matrix and its inverse
## makeCacheMatrix is a function which creates a special "matrix" object that can 
## cache its inverse for the input (which is an invertible square matrix)

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  setmatrix <- function(y){
    x<<- y
    i<<- NULL
  }
  getmatrix <- function() x
  setinverse <- function(inverse) i<<- inverse
  getinverse <- function() i
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinverse  = setinverse, getinverse = getinverse)
}


## This function calculates the inverse of a matrix and caches its value.
## cacheSolve is a function which computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$getmatrix()
  i <-  solve(data, ...)
  x$setinverse(i)
  i
}

## The two functions below handle construction, inversion and caching of matrix objects.

## makeCacheMatrix creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() {
    x
  }
  
  setinverse <- function(inverse) {
    inv <<- inverse
  } 
  
  getinverse <- function() {
    inv
  }
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve computes and returns the inverse of the special matrix.  Assumes matrix is invertable.
cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  
  #Checks for null value and returns cached version if not null
  if (!is.null(inverse)) {
    return(inverse)
  }
  
  #No cached version, need to get the matrix, solve it, and cache the inverted.
  data <- x$get()
  
  inv <- solve(data, ...)
  
  x$setinverse(inv)
  
  inv
}



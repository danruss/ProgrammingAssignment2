## The two functions below handle construction, inversion and caching of matrix objects.

## makeCacheMatrix creates a matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  #Cache the matrix with a Null value for the inverted/solved matrix since it hasn't been computed yet.
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  #Returns the matrix
  get <- function() {
    x
  }
  
  #Sets the inverted value in cache
  setinverse <- function(inverse) {
    inv <<- inverse
  } 
  
  #Retrieves the inverted value from cache (returns NULL if it hasn't been previously set)
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
  
  #No cached inverted matrix: need to get the matrix, solve it (invert), cache the inverted, and return it.
  data <- x$get()
  
  inv <- solve(data, ...)
  
  x$setinverse(inv)
  
  inv
}



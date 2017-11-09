## Provides a set of functions that can be used to calculate the inverse of
## a matrix in a way where the result would be cached if the inverse of the 
## matrix were to be calculated again.

## When given a matrix, outputs an object that when passed into 
## cacheSolve will return the inverse of that matrix and cache the
## result. 
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  
  list(
    set = set,
    get = get,
    setInverse = setInverse,
    getInverse = getInverse
  )
}


## Calls Solve (matrix inverse) on a matrix created with makeCacheMatrix.
## If cacheSolve has been called with this value x already, it uses a cached
## result instead of recalculating the inverse. 
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  
  inv
}

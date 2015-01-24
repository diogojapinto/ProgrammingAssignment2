## Library for optimizing consequent inversion of matrixes
## Caches alonside a matrix its inverse on first calculation

## Function to build a matrix which caches its own inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  get <- function() x
  set <- function(y) x <<- y; inverse <<- NULL
  getInverse <- function() inverse
  setInverse <- function(i) inverse <<- i
  
  list(get = get, set = set, getInverse = getInverse, setInverse = setInverse)
}


## Calculates the inverse of a matrix 'x' built with the makeCacheMatrix function
## If 'x' has the inverse cache, returns it imediatly, else calculates and stores 
## it for future use

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if (!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  inverse <- solve(x$get())
  x$setInverse(inverse)
  inverse
}

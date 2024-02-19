## This function creates a list of the functions used for calculating and caching inverse matrices
makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
set <- function(y) {
  x <<- y
  inv <<- NULL
      }
get <- function() x
setinverse <- function(inverse) inv <<- inverse
getinverse <- function() inv
list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This second function retrieves the inverse matrix if it has already been made before and 
## caches the inverse matrix if it has not been made before

cacheSolve <- cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

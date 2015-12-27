## These functions will cache the calculated inverse of a matrix in
##   case the calculation takes a long time, so that it can look up
##   the data in the cache instead of recalculating.
## We assume the matrix is always invertible.

## makeCacheMatrix creates a list containing functions to:
##     1. set the value of the matrix
##     2. get the value of the matrix
##     3. set the value of the inverse
##     4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
    }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  invisible(list(set = set, get = get, setinv = setinv, getinv = getinv))
}


## cacheSolve calculates the inverse of the matrix
##   first it checks whether this inverse has already been cached

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  ## if the inverse isn't already cached then calculate and return it
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {

    inversa <- NULL
  set <- function(y) {
          x <<- y
          inversa <<- NULL
  }
  get <- function() x
  setInversa <- function(i) inversa <<- i
  getInversa <- function() inversa
  list(set = set,
       get = get,
       setInversa = setInversa,
       getInversa = getInversa)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
inversa <- x$getInversa()
  if (!is.null(inversa)) {
          message("getting cached data")
          return(inversa)
  }
  data <- x$get()
  inversa <- solve(data, ...)
  x$setInversa(inversa)
  inversa
}



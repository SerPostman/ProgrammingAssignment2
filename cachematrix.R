## The following functions are meant to first create
## a matrix with the ability to cache its own inverse
## and then compute its inverse or take it from the cache,
## if it has already been computed.

## The function makeCacheMatrix creates a square matrix with
## the ability to cache its own inverse.
## use like this: xy <- makeCacheMatrix(x=matrix(YOUR MATRIX))

makeCacheMatrix <- function(x = matrix()) {
      s <- NULL
      set <- function(y) {
            x <<- y
            s <<- NULL
      }
      get <- function() x
      setsolve <- function(solve) s <<- solve
      getsolve <- function() s
      list(set = set, get = get,
           setsolve = setsolve,
           getsolve = getsolve)
}


## The cacheSolve function computes the inverse of
## the matrix created with the makeCacheMatrix function.
## If the inverse has already been computed, it returns
## the result from cache.

cacheSolve <- function(x, ...) {
      s <- x$getsolve()
      if(!is.null(s)) {
            message("getting cached data")
            return(s)
      }
      data <- x$get()
      s <- solve(data, ...)
      x$setsolve(s)
      s
}

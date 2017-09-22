## Script to cache the inverse of a matrix.

## This function, 'makeCacheMatrix' will create a list of functions to set a matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}

## This function 'cacheSolve' solves for a matrix's inverse if it is not cached before.
cacheSolve <- function(x, ...) {
       m <- x$getinverse()
       if(!is.null(m)) {
         message("Getting CACHED DATA")
         return(m)
       }
       data <- x$get()
       m <- solve(data, ...)
       x$setinverse(m)
       m
}

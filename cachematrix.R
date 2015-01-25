## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix contains functions to set and get value of a matrix, 
## set and get value of inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinv <- function(solve) s <<- solve
  getinv <- function() s
  list(set = set, get = get, setinv = setinv,
       getinv = getinv)
}


## cacheSolve calculates inverse of a matrix created by makeCacheMatrix
## if inverse was computed above, calculation is skipped.
## Inverse of 'x' is returned

cacheSolve <- function(x, ...) {
  s <- x$getinv()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinv(s)
  s
}

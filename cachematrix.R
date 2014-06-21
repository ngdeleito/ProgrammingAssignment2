## The functions defined in this file allow to compute the inverse of a matrix
## in such a way that the result is cached (i.e. the inverse is only computed
## once).
## Usage:
## m <- makeCacheMatrix(matrix(c(2, 6, 4, 8), nrow=2, ncol=2))
## cacheSolve(m) # in this first call the inverse is actually computed
## cacheSolve(m) # in this second call the value computed previously is returned

## "makeCacheMatrix" creates an environment with a matrix "inverse", a matrix
## "matrix" and a list containing four functions ("set", "get", "getInverse",
## and "setInverse").
## "set" and setInverse" modify the value of "inverse" and/or "matrix" thanks to
## the <<- operator. Note that if <- would be used instead, variables local to
## "set" and "setInverse" would then be defined, and the "inverse" and "matrix"
## variables in the parent environment would not be modified.

makeCacheMatrix <- function(matrix = matrix()) {
  inverse <- NULL
  set <- function(newMatrix) {
    matrix <<- newMatrix
    inverse <<- NULL
  }
  get <- function() matrix
  getInverse <- function() inverse
  setInverse <- function(inverseValue) inverse <<- inverseValue
  list(set = set, get = get, getInverse = getInverse, setInverse = setInverse)
}

## "cacheSolve" is a function that receives an environment as defined by
## "makeCacheMatrix" above and returns the inverse of the matrix associated to
## this environment.
## The actual computation of the inverse only happens when this function is
## called with a given environment for the first time. Subsequent calls with
## that same environment reuse the value computed previously (which is
## cached/stored in this environment).
## The matrices given to this function are assumed to be invertible.

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if (!is.null(inverse)) {
    message("getting cached matrix")
    return(inverse)
  }
  inverse <- solve(x$get())
  x$setInverse(inverse)
  inverse
}
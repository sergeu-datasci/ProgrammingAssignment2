## The following two functions allow to cache and reuse the inverse of a matrix
## instead of computing it repeatedly.
##
## Usage:
##  1. Use 'makeCacheMatrix(x)' function, where 'x' is a square invertible
##     matrix, to instantiate a cache object for the matrix and its inverse.
##     The cache object is actually a list of 'set' and 'get' functions which
##     handle that cache values.
##  2. Use 'cacheSolve(x, ...)' function, where 'x' is a variable
##     referencing to the list of cache handle functions, to compute the inverse
##     of the cached matrix.

## makeCacheMatrix(x = matrix()):
##  This function instantiates a cache object for the matrix
##  and its inverse. It returns a list of 'set' and 'get' functions to
##  handle the matrix and its inverse cached values:
##     1) 'set': to set the matrix value
##     2) 'get': to get the matrix value
##     3) 'setinv': to set the inverse matrix value
##     4) 'getinv': to get the inverse matrix value

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    ## Return the list of setters and getters for the matrix and its inverse
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## cacheSolve(x, ...):
##  This function takes a cache object (which is actually a list of 'set' and
##  'get' functions for its elements) and returns the inverse of the cached
##  matrix. If the cached inverse value does not exist, it does calculate the
##  invers and store it in the cache.

cacheSolve <- function(x, ...) {
    inverse <- x$getinv()
    if (!is.null(inverse)) {
        message("getting cached data")
        return (inverse)
    }
    matrix <- x$get()
    inverse <- solve(matrix)
    x$setinv(inverse)
    ## Return a matrix that is the inverse of 'x'
    inverse
}

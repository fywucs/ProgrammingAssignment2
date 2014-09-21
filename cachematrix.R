## Functions to create a persistent matrix capable of caching
## its inverse after initial calculation for retrieval
## on subsequent request.

## These functions avoid repeated expensive calculation of the inverse
## of an invertible square matrix.

## Utility function to create a CacheMatrix to store a matrix
## and its inverse. Upon initialisation the inverse is set to
## NULL. Functions are included to set and get the matrix and
## its inverse.
## NOTE: this function assumes the matrix is square invertible

makeCacheMatrix <- function(m = matrix()) {
        i <- NULL
        set <- function(y) {
                m <<- y
                i <<- NULL
        }
        get <- function() m
        setInverse <- function(inverse) i <<- inverse
        getInverse <- function() i
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Function that takes a CacheMatrix instance and tests
## to see if an inverse is currently cached for the matrix
## and returns this inverse if it exsists. If the inverse
## does not exist, the inverse is calculated using the solve()
## function, written to the CacheMatrix and returned by the
## function.
## NOTE: this function assumes the matrix to be square invertible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}

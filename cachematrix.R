# The makeCacheMatrix and cacheSolve functions work together to compute the inverse of 
# a matrix and store the value in a cache (object state) 

## This function creates a special "matrix" object that can cache its inverse.
## It returns a list with getters and setters for the matrix and the inverse.
makeCacheMatrix <- function(x = matrix()) {
    # in this variable we'll store the cached value
    inverse <- NULL

    get <- function() x
    set <- function(y) {
        x <<- y
        # matrix has chaned => invalidate the "cache"
        inverse <<- NULL
    }

    setInverse <- function(i) inverse <<- i
    getInverse <- function() inverse

    list(set=set, get=get, 
         setInverse=setInverse,
         getInverse=getInverse)
}


## Given a "cacheMatrix" list, returns the inverse of the matrix.
## First it tries to get the inverse from the cache, but if the value is not cached, 
## it computes the inverse and stores it in the cache before returning it.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()

    if (!is.null(inverse)) {
        message("Getting cached data...")
        return(inverse)
    }
    message("Computing inverse...")
    # inverse value not in cache => compute it & store it
    invertible_matrix = x$get()
    inverse <-solve(invertible_matrix)
    x$setInverse(inverse)

    inverse
}


## USAGE:
regular_matrix = matrix(c(1,3,2,4), 2, 2)
special_matrix = makeCacheMatrix(regular_matrix)
# solve inverse
inverse1 = cacheSolve(special_matrix)
# retrieve it from "cache"
inverse2 = cacheSolve(special_matrix)

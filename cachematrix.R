## This file contains functions to create and 
## apply a specialized matrix, with the ability
## of caching it's calculated inverse for later
## retrievel.


## Function to create a matrix able to cache it's
## inverse, once calculated. Input matrix must
## be square and invertable.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        a <<- y
        m <<- NULL
    }
    get <- function() a
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Function to calculate the inverse of a matrix,
## using a cached result if available.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("returning cached inverse matrix")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

## These two functions will set a matrix, calculate its inverse,
## and cache the inverse to avoid the need to re-calculate
## the inverse. If the matrix has changed, the inverse will be
## recalculated and cached.

## Create a special "matrix" that is really a list of
## functions to:
##      set the value of a matrix
##      get the matrix
##      set the value of the inverse
##      get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL # initialize the matrix with NULL inverse
        set <- function(y) { #set (or change) the matrix, and...
                x <<- y # ...make available outside of set
                m <<- NULL # ...reset the cached inverse to NULL
        }
        get <- function() x # return the matrix
        setInverse <- function(solve) m <<- solve # save inverse value
        getInverse <- function() m # return inverse value
        
        # have the function return a list of functions
        
        list(set = set, get = get, setInverse = setInverse,
             getInverse = getInverse)
}


## This function will calculate the inverse
## of the "matrix" created in the function above.
## If the inverse has already been calculated
## and the "matrix" is unchange, it will return
## the cached value. If the inverse has not been
## calculated or the "matrix" has changed, it will
## calculate the inverse.

cacheSolve <- function(x, ...) {
        
        # get the cached inverse, if not NULL
        
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        # if the cached inverse is NULL, then 
        # calculate the inverse and return it
        
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}

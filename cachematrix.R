## Functions for caching inverse of a matrix to avoid costly re-calculation.

## Creates a list of functions to set/get the matrix and set/get its invertible
## Setting a new matrix will also set to NULL its invertible, so if invertible 
## is not NULL, cached value can be used. Whenever it is NULL, it has to be 
## re-calculated, and re-set. 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        # gets the matrix
        get <- function() {
                x
        }
        # sets the matrix, resets inv to null
        set <- function(m) {
                x <<- m
                inv <<- NULL
        }
        # gets inverse matrix
        getinv <- function() {
                inv
        }
        # sets inverse matrix
        setinv <- function(minv) {
                inv <<- minv
        }
        # return list of functions
        list(get = get, set = set, getinv = getinv, setinv = setinv)
}


## Inverts the given matrix, but if matrix has not changed, 
## returns the cached inverse without re-doing the calculation.
## x is of type returned by the makeCacheMatrix function, i.e. list.
## The other parameters are as in the solve(x, ...) function.

cacheSolve <- function(x, ...) {
        # check inverse is not null (cached value exists)
        inv <- x$getinv()
        if (!is.null(inv)) {
             # return cached value
             return(inv)
        }
        # do calculate the inverse, store in cache and return
        inv <- solve(x$get(), ...)
        x$setinv(inv)
        inv
}

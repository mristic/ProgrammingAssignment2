## makeCacheMatrix and cacheSolve functions enable storing a matrix and 
## caching its inverse, so the calculation of the inverse can be skipped
## if it has been already calculated and the inverse is retrieved from
## the cache instead.

## makeCacheMatrix creates a special object that stores a matrix
## and caches its inverse. This special object is actually a list
## containing functions to:
##     set the value of the matrix
##     get the value of the matrix
##     set the value of the inverse of the matrix
##     get the value of the inverse of the matrix

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


## cacheSolve returns the inverse of the matrix. It checks if the inverse
## has already been calculated and if so, it skips the computation and
## returns the cached value of the inverse. Otherwise, it calculates the
## inverse of the matrix and stores it in the cache using setsolve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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

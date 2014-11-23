## This function makes Cache for a matrix x
makeCacheMatrix <- function(x = matrix()) {
## m is a variable to store Cached inverse matrix
        m <- NULL
## set function is to refresh matrix x and stored inverse matrix m when a different matrix is calculated
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
## get function is to obtain cached matrix x
        get <- function() x
## setsolve is to store the inverse matrix to m
        setsolve <- function(solve) m <<- solve
## getsolve is to obtain stored inverse matrix m
	getsolve <- function() m
## the output of makeCacheMatrix is a list of 4 functions
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## cache solve will calculate inverse of the matrix, if it is calculated and Cached, it will retrieve the cached value.

cachesolve <- function(x, ...) {
## get the cached inverse matrix
        m <- x$getsolve()
## if m is not empty, just use the cached value
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
##otherwise return the matrix to the one we want to invert
        data <- x$get()
## calculate the inverse, store to m
        m <- solve(data, ...)
## store m to the cached value
        x$setsolve(m)
## return the function to m
        m
}
## Return a matrix that is the inverse of 'x'


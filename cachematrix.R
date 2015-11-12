## The makeCacheMatrix and cacheSolve functions work together to
## preserve (cache) the inverse of a matrix for later reference.

## makeCacheMatrix takes a matrix parameter (or creates an empty
## matrix) and returns a list of functions that it has defined on
## that matrix.  The matrix and its inverse can be accessed through
## these methods.  The use of the `<<-` assignment operator ensures
## that the changes are propagated to the parent environment

makeCacheMatrix <- function(x = matrix()) {
        imat <- NULL
        set <- function(y) {
            x <<- y
            imat <<- NULL
        }
        get <- function() x
        setinv <- function(inv) imat <<- inv
        getinv <- function() imat
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve checks to see if the inverse of the parameter matrix
## is cached using the getinv() function.  If cached, it gets the
## cached version and returns it.  If not cached, solve() is used
## find the inverse and setinv() is used to cache it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        imat <- x$getinv()
        if(!is.null(imat)) {
               message("getting cached inverse")
               return(imat)
        }
        data <- x$get()
        imat <- solve(data, ...)
        x$setinv(imat)
        imat
}

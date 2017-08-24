## This function creates a object that stores a matrix 
## and can caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
    minv <- NULL  
    set <- function(y) {
        x <<- y
        minv <<- NULL
    }
    get <- function() x
    setInvk <- function(invk) minv <<- invk  # set inverse matrix
    getInvk <- function() minv
    list(set = set,
             get = get,
             setInvk = setInvk,
             getInvk = getInvk)
}

## calc matrix_inv If matrix_inv not calc 
## and the matrix has not changed, 
## or else it should retrieve from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix_inv 'x'
    minv <- x$getInvk()
    if (!is.null(minv)) {
        message("getting cached data")
        return(minv)
    }
    dmat <- x$get()
    minv <- solve(dmat, ...)
    x$setInvk(minv)
    minv
}
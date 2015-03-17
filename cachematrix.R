## The following pair of functions cache the inverse of a matrix.
## In many circumstances, matrix inversion can be computationally
## costly, and these functions offer the benefit of caching the
## inverse of a matrix rather than having to compute it repeatedly.

## This function implements a special "matrix" object that can
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    
    # initialize the cache to NULL
    m <- NULL
    
    # set the matrix
    set <- function(y) {
        x <<- y
        # since the matrix has changed, the cache is set to null
        m <<- NULL
    }
    
    # get the matrix
    get <- function() x
    
    # cache the inverse
    setinverse <- function(inverse) m <<- inverse
    
    # retrieve the cached inverse
    getinverse <- function() m
    
    # return a list of functions
    list(set = set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}


## This function computes the inverse of the matrix returned by
## the makeCacheMatrix function above. Where the inverse has been
## previously calculated and the matrix has not changed, the 
## function will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    
    ## get the cached inverse of 'x'
    m <- x$getinverse()
    
    ## return the cached inverse if it exists
    if(!is.null(m)) {
        message("getting cached data")
        return(m) 
    }
    
    ## if it does not exist, get the matrix, compute the inverse
    ## and set the cache
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    
    ## return the inverse
    m
}

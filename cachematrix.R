## PROGRAM DESCRIPTION :
## makeCacheMatrix creates a special matrix object, and then what the cacheSolve
## does is it calculates the inverse of the matrix. If the matrix inverse has
## already been calculated, it will instead retrieve it from the cache and
## return it, and not calculate it again.


## The function makeCacheMatrix creates a special "matrix" object that can cache
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The function cacheSolve computes the inverse of the special "matrix" returned
## by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}



makeCacheMatrix <- function(x = matrix()) {
    # variable for caching the inverse matrix
    inverse <- NULL

    # returns current matrix 
    get <- function() {
        x
    }
    
    # set new matrix 
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }

    # returns current inverse matrix
    getinverse <- function() {
        inverse
    }
    
    # set new inverse matrix
    setinverse <- function(newInverse) {
        inverse <<- newInverse
    }    
    
    # return previously defined functions as list
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}



cacheSolve <- function(x, ...) {
    # try to get cached inverse
    inverse <- x$getinverse()
    
    # if inverse was not found from cache, compute it now and add to cache
    if (is.null(inverse)) {
        inverse <- solve(x$get())
        x$setinverse(inverse)
    }

    return(inverse)
}

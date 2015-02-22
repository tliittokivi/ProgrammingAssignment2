# Functions to compute matrix inversion once and cache it's value for repeated calls
#
# Example:
# > m = makeCacheMatrix(matrix(c(5,5,6,7),2,2))
#
# > m$get()
# [,1] [,2]
# [1,]    5    6
# [2,]    5    7
#
# > m$getinverse()
# NULL
#
# > i = cacheSolve(m)
# > i
# [,1] [,2]
# [1,]  1.4 -1.2
# [2,] -1.0  1.0
#
# > identical(i, m$getinverse())
# [1] TRUE




# Returns list of getter/setter functions that can be used to store matrix and it's inverse matrix
#
# Arguments:
# x    A square numeric or complex matrix
#
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


# Returns inverse of given matrix. If inverse has been computed before for this object, 
# cached value is returned.
#
# Arguments:
# x    Matrix object created with makeCacheMatrix function
#
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

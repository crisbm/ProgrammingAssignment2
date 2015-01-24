## makeCacheMatrix is a function that creates a list containing the following
## functions:
### set() -- sets the value of the Matrix
### get() -- gets the value of the Matrix
### setInverse() -- sets the value of the Inverse of the Matrix
### getInverse() -- gets the value of the Inverse of the Matrix

makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(i) inv <<- i
        getInverse <- function() inv
        list(set = set, get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)
}


## cacheSolve calculates the Inverse of a Matrix using teh functions of the 
## previous function. 
## It first checks if the Inverse has already been calculated. If so, it gets
## that value from the cache and skips the rest. Otherwise, it calculates the 
## Inverse of the matrix and, before returns it, it saves that caculation in the
## cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("Getting the Inverse of the Matrix from cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setInverse(inv)
        inv
}

## These functions take the sometimes time-consuming task of
## calculating the inverse of a matrix, and ensure that while
## the underlying matrix remains unchanged, the calculation
## of the inverse need only happen once.

## This function takes a matrix as an argument, and contains
## getters and setters for both the matrix itself, and the
## inverse of the matrix. When the matrix is changed the
## cache of the inverse matrix is cleared

makeCacheMatrix <- function(x = matrix()) {
    # I'll confess some minor scoping confusion.  I don't
    # really understand what environment inverse is being
    # created in and why it doesn't end up being shared
    # by all instances of makeCacheMatrix, but I tested
    # and it doesn't
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() {
        x
    }
    setInverse <- function(invertedMatrix) {
        inverse <<- invertedMatrix
    }
    getInverse <- function() {
        inverse
    }
    # this list below was confusing to me before so I'm commenting
    # to explain it to future me.  The last call in a function
    # is the one that gets returned, so by returning this list,
    # we are providing all the functions to the new object
    # being created.
    list(set = set, get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)
}


## This function takes an object created with makeCacheMatrix
## as an argument and returns the inverse of its matrix, from
## cache if it exists, and via calculation that is then cached
## if not.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invertedMatrix <- x$getInverse()
    if(!is.null(invertedMatrix)) {
        message("getting cached data")
        return(invertedMatrix)
    }
    data <- x$get()
    invertedMatrix <- solve(data)
    x$setInverse(invertedMatrix)
    invertedMatrix
}

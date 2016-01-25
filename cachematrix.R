# R programming - assignment 2 - Caching the Inverse of a Matrix
#
# sample usage:
#
# some_matrix <- rbind(c(1, 2), c(2, 1))
# x <- makeCacheMatrix(some_matrix)
# cacheSolve(x)
# cacheSolve(x)
#
# the first call to cacheSolve will calculate the results, the second one will
# use the cache


# makeCacheMatrix creates a list that contains functions to:
# - set the value of the matrix
# - get the value of the matrix
# - set the value of the inverse of the matrix
# - get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix <- NULL

    set <- function(y) {
        x <<- y
        inverseMatrix <<- NULL
    }

    get <- function() {
        x
    }

    setInverseMatrix <- function(inv) {
        inverseMatrix <<- inv
    }

    getInverseMatrix <- function() {
        inverseMatrix
    }

    list(set = set, get = get, setInverseMatrix = setInverseMatrix, getInverseMatrix = getInverseMatrix)
}


# return the inverse of the matrix
# x is an object returned by the makeCacheMatrix function
cacheSolve <- function(x, ...) {

    # check if a cached result exists
    inverseMatrix <- x$getInverseMatrix()
    if (!is.null(inverseMatrix)) {
        # use the cached results
        message("using cached results")
        return(inverseMatrix)
    }

    # no cached results found - calculate the inverse
    message("no cached result found - using solve")
    data <- x$get()
    inverseMatrix <- solve(data, ...)

    # cache the inverse
    x$setInverseMatrix(inverseMatrix)

    return(inverseMatrix)
}

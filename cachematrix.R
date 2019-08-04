## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a list of 4 functions that can cache the
## inverse of invertible matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setSolve <- function(inv) i <<- inv 
        getSolve <- function() i
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}


## cacheSolve first checks whether the inverse of a matrix has been computed
## or not. If it has been computed before, it returns the value of prior
## computation. If not, it computes the inverse of the matrix by the solve()
## function, assuming the matrix is square and invertible.

cacheSolve <- function(x, ...) {
        i <- x$getSolve()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        my_matrix <- x$get()
        i <- solve(my_matrix, ...)
        x$setSolve(i)
        i
        ## Return a matrix that is the inverse of 'x'
}
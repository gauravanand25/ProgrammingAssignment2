## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than computing it repeatedly

## This function return the list containing the functions and the matrix in it's environment

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(z) {
        x <<- z     # can set a new value 
        inverse <<- NULL
    }
    get <- function() x # return the matrix
    setinverse <- function(val) inverse <<- val # stores the val passed to the inverse
    getinverse <- function() inverse # return inverse
    list(get = get, set = set, getinverse = getinverse, setinverse = setinverse)
}


## This function return the inverse of a matrix

cacheSolve <- function(y, ...) {
    inverse <- y$getinverse()
    if( !is.null(inverse) ) {
        message("returning cached inverse")
        return(inverse)
    }
    mat <- y$get()
    inverse <- solve(mat, ...) # extra arguments like LINPACK = TRUE etc. are passed as it is.
    y$setinverse(inverse)
    inverse
}

## Below is an example of using these functions
## x <- matrix(rnorm(9), 3, 3)
## y <- makeCacheMatrix(x)
## cacheSolve(y) # it would compute and store the value in the environment
## cacheSolve(y) # this would return a cached value


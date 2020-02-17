## This function creates a special "matrix" object that can cache its inverse.

## The first function creates a list containing a function to:
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of inverse of the matrix
# 4. Get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    setmat <- function(y) {
        x <<- y
        m <<- NULL
    }
    getmat <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(setmat = setmat, getmat = getmat, setinv = setinv, getinv = getinv)
}



## The following function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# It first check if the inverse has already been computed. If it was, then should retrieve the inverse 
# from the cache.
# This function  assumes that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cache data")
        return(m)
    }
    data <- x$getmat()
    m <- solve(data, ...)
    x$setinv
    m
}
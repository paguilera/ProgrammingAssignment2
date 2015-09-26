## In this file, you will find two functions. The first is the makeCacheMatrix
## function and the second is the cacheSolve function. The first function
## handles the caching aspects and is not meant for direct use by the user and
## the second function is the user-facing function that the user uses to query
## a matrix for an inverse and retrieves the cached version if it is available.

## This function is meant to store the cached information about a given matrix.
## This includes functions to check for the inverse of the matrix as well as
## allowing the user to save the inverse of the matrix for future use.
## Note: This is not a user-facing function. The user should be using the
## cacheSolve function instead.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## This is a user-facing function that allows the user to query a matrix for
## the inverse of the matrix. Should that inverse not be cached already, the
## function then uses the makeCacheMatrix function to cache it for future use.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- makeCacheMatrix(x)$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- makeCacheMatrix(x)$get()
    m <- solve(data, ...)
    makeCacheMatrix(x)$setsolve(m)
    m
}

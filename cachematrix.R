## The functions below get a matrix, which is subsequently is inverted.
## Note! The matrix which is used must be square and invertible.

## This function creates a list of functions which allow:
## - set and store matrix with the option of change it
## - set an inverted matrix and store it
## - store an inverted matrix which return from cacheSolve functions

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}
## This function works with result of 'makeCacheMatrix' function
## Match better if result of 'makeCacheMatrix'looks like a variable
## it checks for the presence of the inverted matrix
## if exist - function return it
## if not - calculate an inverted matrix, store result in 'x' argument and return it
cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
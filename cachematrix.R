## These functions allow us to efficiently retrieve the inverse of
## a matrix by caching results in the global environment. To use 
## this we must follow a two step process:
##
## 1. Call the function makeCacheMatrix() passing in the matrix
##    you wish to get the inverse for as an argument.
## 2. Call the function cacheSolve(), passing in the list
##    that was return in step 1. This will either return the
##    cached calculation of the matrix inverse (and print a 
##    message indicating so) or calculate it and return it.

## Function that returns a list of functions that are used to
## manage matrix inverses in the global environment. Functions
## are as follows:
##   * get() - Gets the matrix
##   * set() - Sets the matrix
##   * setinverse() - Sets the inverse of the matrix
##   * getinverse() - Gets the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    # initialize the inverse to NULL
    i <- NULL
    # initialize the matrix value and reset i
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    # Getter for the matrix
    get <- function() x
    # Setter for the matrix inverse
    setinverse <- function(inverse) i <<- inverse
    # Getter for the inverse
    getinverse <- function() i
    # Return a list (i.e. special "matrix" object) with the functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse= getinverse)
}

## Function that returns a list of functions that are used to
## manage matrix inverses in the global environment. Functions
## are as follows:
##   * get() - Gets the matrix
##   * set() - Sets the matrix
##   * setinverse() - Sets the inverse of the matrix
##   * getinverse() - Gets the inverse of the matrix
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse (i) of x
    i <- x$getinverse()
    ## If we already calculated i, print a message that we
    ## are using the cached value then return it
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    ## If we haven't calculated the inverse yet, grab the matrix,
    ## calculate its inverse, set the cached value, the return it
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}

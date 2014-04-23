## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "vector", which is a list containing a function to
##   1) set: set the value of the matrix
##   2) get: get the value of the matrix
##   3) setinverse: set the value of the inverse matrix
##   4) getinverse: get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
## This function calculates the inverse matrix of the matrix created with the "makeCacheMatrix" function. 
##      1) it first checks to see if the inverse matrix has already been calculated. 
##           1a) If so, it gets the inverse matrix from the cache and skips the computation. 
##           1b) Otherwise, it calculates the inverse matrix of the data 
##      2) it sets the value of the inverse matrix in the cache via the "setinverse" function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}

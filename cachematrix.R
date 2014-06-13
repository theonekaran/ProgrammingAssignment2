## The two functions below are used to create a special object that stores a  
## matrix and cache's its inverse using the R command solve.

## This function below, makeCacheVector creates a special "matrix", 
## which is really a list containing a function to:
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse using the solve command
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## The following function calculates the inverse of the special "matrix" 
## created with the makeCacheMatrix function. In doing so, it first checks to see 
## if the inverse already exists in the cache. If so, it gets the cached inverse 
## and skips the computation. 
## Otherwise, it calculates the inverse of the data and 
## stores the value of the inverse matrix in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}

## Functions to cache the inverse of a matrix

## This function creates a special matrix object that 
## can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL                
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Function to compute the inverse of the matrix returned by 
## makeCacheMatrix, or retrieve the inverse from the cache if 
## the inverse had already been calculated

cacheSolve <- function(x, ...) {
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

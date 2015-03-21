## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    setCacheMatrix <- function(y)
    {
        x <<- y
        inv <<- NULL
    }
    getCacheMatrix <- function() x
    setCacheInverse <- function(z) inv <<- z
    getCacheInverse <- function() inv
    list (setCacheMatrix = setCacheMatrix, 
          getCacheMatrix = getCacheMatrix, 
          setCacheInverse = setCacheInverse, 
          getCacheInverse = getCacheInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getCacheInverse()
    if (!is.null(inv))
    {
        message ("getting cached matrix inverse")
        return inv;
    }
    mat <- x$getCacheMatrix()
    inv <- solve(mat, ...)
    m$setCacheInverse(inv)
    return inv
}

## Put comments here that give an overall description of what your
## functions do


## Creates a function (structure) that takes  matrix that holds the inverse of the matrix
## and contains functions to get and set the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    setCacheMatrix <- function(y)
    {
        #assign matrix either if it is empty or is different from existing
        if (!is.na(x) || !identical(y,x))
        {
            x <<- y
            inv <<- NULL
        }
    }
    
    ## returns the matrix
    getCacheMatrix <- function() x
    
    ## saves the passed inverse of matrix
    setCacheInverse <- function(z) inv <<- z
    
    ## returns the saved inverse of matrix
    getCacheInverse <- function() inv
    
    ## list of functions to manipulate the saved matrix and its inverse
    list (setCacheMatrix = setCacheMatrix, 
          getCacheMatrix = getCacheMatrix, 
          setCacheInverse = setCacheInverse, 
          getCacheInverse = getCacheInverse)
}


## Takes CacheMatrix object as input ad returns the inverse if that is already
## computed and stored, else calculates a new one and stores it for future use

cacheSolve <- function(x, ...) {
    ## checks whether an inverse is already available
    inv <- x$getCacheInverse()
    if (!is.null(inv))
    {
        message ("getting cached matrix inverse")
        ## returns the available inverse
        return inv;
    }
    
    ## if no existing inverse available then gets the matrix from the object
    ## calculates the inverse and stores it back to the object
    mat <- x$getCacheMatrix()
    inv <- solve(mat, ...)
    x$setCacheInverse(inv)
    
    return inv
}
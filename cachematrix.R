## Matrix inversion is usually a costly computation and their may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly.
## This file contains two functions 'makeCacheMatrix' and 'cacheSolve' to calculate and cache the 
## inverse of the given matrix


## makeCacheMatrix
##  This function makes a matrix object with the given matrix
##  The matrix object has four sub function to perform the following operations
##  set        -- sub function to set a new value to the matrix object,
##                it will also make the old cached value invalid.
##  get        -- sub function to retrive the matrix value from the matrix object
##  setInverse -- sub function to set the inverse matrix
##  getInverse -- sub function to get the cached inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        cachedMatrix <- NULL
        
        set <- function(matrix) {
                x <<- matrix
                cachedMatrix <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) cachedMatrix <<- inverse
        getInverse <- function() cachedMatrix
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve
## This function returns the inverse of a matrix which is cached in the matrix object.
## If matrix object doesn't have a valid cache value then this function will calculate 
## inverse of the matrix and also set the newly calculated inverse in the cache of the 
## matrix object.
##
## Note: This function assumes that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
        ## get the inverse from matrix object cache
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached inverse")
                return (inv)
        }
        matrix <- x$get()
        inverse <- solve(matrix, ...)
        ## set the newly calculated inverse in the matrix object cache
        x$setInverse(inverse)
        ## Return the inverse of 'x'
        inverse
}

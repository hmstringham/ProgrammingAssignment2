## Programming Assignment 2: Lexical Scoping

## Matrix inversion can be computationally intensive and therefore it may be beneficial to cache the inverse
## of a matrix rather than computing it repeatedly. This assignment contains a pair of functions that cache 
## the inverse of a matrix.

## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inv.calc) inv <<- inv.calc
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## The cacheSolve function calculates the inverse of the special "matrix" object returned by the makeCacheMatrix 
## function. It checks to see if the inverse has already been calculated. If so, it does not caculate the inverse, 
## but rather it prints the message "getting cached data" and gets the inverse from the cache.  Otherwise, it 
## calculates the inverse of the matrix and sets the value of the inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x' where x is an object from makeCacheMatrix.
    inv.local <- x$getInverse()
    if(!is.null(inv.local)) {
        message("getting cached data")
        return(inv.local) # return the cached inverse of the matrix
    }
    data <- x$get()
    inv.local.calculated <- solve(data, ...)
    x$setInverse(inv.local.calculated)
    inv.local.calculated # return the calculated inverse of the matrix
}

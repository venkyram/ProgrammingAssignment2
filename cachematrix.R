## These functions are for caching the inverse of a matrix
## This function creates a special matrix object that can cache its inverse
##
## How do you test this?
## 1. Clear your R env. i.e. Execute 
##      rm(list =ls())
## 2. Load the cachematrix.R. i.e. Execute 
##      source("cachematrix.R")
## 3. Create a 4x4 matrix by using rnorm distribution
##    i.e. Execute 
##      xx <- matrix(rnorm(16, 10, 1), 4, 4)
## 4. Cache the matrix
##    i.e. Execute 
##      yy <- makeCacheMatrix(xx)
## 5. Now calculate the inverse by calling cacheSolve
##    i.e. Execute
##      cacheSolve(yy, xx)
##    If the original matrix is non solvable, then this will return an error.
##    That is not a programming error. That is the nature of matrices.
##    In that case try another matrix which is solvable (i.e. invertable)
##    If you try the same call #5 again, it should typically result with a 
##    message "getting cached inverse", for the same matrix xx.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(minverse) m <<- minverse
        getinverse <- function() m
        list (set = set, 
              get = get,
              setinverse = setinverse,
              getinverse = getinverse)
}


## This function computes the inverse of the special matrix returned by the 
## above function. If the inverse has already been calcuated (and the matrix
## has not changed), then this should retrieve the inverse from the cache

cacheSolve <- function(x, y, ...) {
        ## 'x' is the cached matrix with the inverse list
        ## 'y' is the current matrix passed in
        
        ## Check if cached matrix 'x' and the current matrix 'y' are different
        ## if different, set the new matrix value and reset the inverse
        if (!identical(x$get(), y)) {
                x$set(y)
        }
        
        ## Get the cached inverse and if not null, return the value
        m <- x$getinverse()
        if (!is.null(m)) {
                message("getting cached inverse")
                return(m)
        }
        
        ## If not get the matrix and calculate the inverse and cache it
        ## before returning
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}

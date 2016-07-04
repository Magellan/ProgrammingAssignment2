## ============================================================================================
## Put comments here that give an overall description of what your
## functions do:
##
## This file has a pair of functions that cache the inverse of a matrix.
##
##
##  makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##  cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
##    above. If the inverse has already been calculated (and the matrix has not changed), then the 
##    cachesolve should retrieve the inverse from the cache.
##
## Submitted by:   Nelson Tan
## File History:
## Date        Contributor          Notes
## ----------  -------------------  ----------------------------------------------------------
#  07/03/2016  Nelson Tan	    Initial coding
## ============================================================================================



## --------------------------------------------------------------------------------------------
## This function creates a special "matrix" object that can cache its inverse
## --------------------------------------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {
   ## This function creates a special "matrix" object that can cache its inverse

   # initialize object
   inverse_data <- NULL

   # set the matrix
   set <- function(y) {
      x <<- y
      inverse_data <<- NULL
   }

   # get the matrix
   # set the matrix inverse
   # get the matrix inverse
   get <- function() x
   setinv <- function(inverse) inverse_data <<- inverse 
   getinv <- function() inverse_data 

   # generate the list. This will be the input to another function
   list(  set = set 
        , get = get 
        , setinv = setinv 
        , getinv = getinv
       )
}


## --------------------------------------------------------------------------------------------
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## --------------------------------------------------------------------------------------------
cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'

   # get the inverse 
   inverse_data <- x$getinv()
        
   # check if the inverse was calculated. If so, use it as the return object
   if (!is.null(inverse_data)){ 
           message("getting cached data")
           return(inverse_data)
   }
        
   # If inverse not cached, calculate it. 
   matrix_data <- x$get()
   inverse_data <- solve(matrix_data, ...)
        
   # set the inverse value
   x$setinv(inverse_data)
       
   return(inverse_data)
}
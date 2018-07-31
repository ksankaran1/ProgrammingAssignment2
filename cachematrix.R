## Programming Assignment 2 
## The below functions compute the inverse of a matrix and caches the value 
## if the matrix values are not changed, instad of re-computing the inverse, it returns the cached value
## It makes use of "closures" i.e. function returns another function as its value
## also << is used to access and manage variables in parent levels

## makeCacheMatrix creates a special matrix object to
## set the value of the matrix
## get the value of the matrix
## set the inverse of the matrix
## get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL

     ## sets the value of the matrix
     set <- function(y) {
          x <<- y
          m <<- NULL
     }

     ## get the value of the matrix
     get <- function() x

     ## set the inverse of the matrix
     setinverse <- function(solve) m <<- solve

     ## get the inverse of the matrix
     getinverse <- function() m

     ## prepare the functions as a list to be accessible by other functions
     list( set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve calculates the inverse of the special matrix object
## created by the above function (makeCacheMatrix)
## it checks if the inverse of the matrix is already available in cache using "getinverse"
## if available, it gets the cached value and returns it
## if not, calculates the inverse of the matrix using function "solve" and sets the value using "setinverse" 

cacheSolve <- function(x, ...) {
    ## attempt to get the inverse of the matrix
    m <- x$getinverse()

    ## if the inverse value is already cached and found, return it
    if (!is.null(m)) {
       message("getting cached data")
       return(m)
    }

   ## this section is executed if "m" is still null,
   ## i.e., first time execution when value is not cached yet
   
   ## invoke get call to read the matrix value
   data <- x$get()
   
   ## compute inverse of the input matrix
   m <- solve(data,...)

   ## set the value
   x$setinverse(m)

   ## return value
   m
}


##-------------------------
### Testing
##-------------------------
## source("cachematrix.R")
## myVar <- makeCacheMatrix(matrix(c(4,2,7,6),2,2))
## myVar$get() 
##     [,1] [,2]
##[1,]    4    7
##[2,]    2    6

## cacheSolve(myVar)
##     [,1] [,2]
## [1,]  0.6 -0.7
## [2,] -0.2  0.4
 
## cacheSolve(myVar)
## getting cached data
##     [,1] [,2]
##[1,]  0.6 -0.7
##[2,] -0.2  0.4

##-------------------------

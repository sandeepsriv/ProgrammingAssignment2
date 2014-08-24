## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function creates a special "matrix" object that can cache its inverse
## If the matrix inverse has already been calculated, it will instead 
## find it in the cache and return it, and not calculate it again.

makeCacheMatrix <- function(x = matrix()) {

    inverse_x <- NULL
    set <- function(y) {
        x <<- y
        inverse_x <<- NULL
    }
    get <- function() x
    setinverse<- function(inverse) inverse_x <<-inverse
    getinverse <- function() inverse_x
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
## cacheSolve returns the inverse of a matrix A created with the previous makeCacheMatrix function.
## If the cached inverse matrix is available, this function will retrieves it else it computes then caches and returns it.


cacheSolve <- function(x, ...) {
    
  ## Return a matrix that is the inverse of 'x'	
  inverse_x <- x$getinverse()
  
  if (!is.null(inverse_x)) {
    ## get cached inverse matrix    
    return (inverse_x)
  }
 else {
   inverse_x <- solve(x$get())
   x$setinverse(inverse_x)
   return(inverse_x)
 }
}

##  Test Cases

## > a<-makeCacheMatrix()

## > a$set(matrix(4:7,2,2))
## >  cacheSolve(a)
## [,1] [,2]
## [1,] -3.5    3
## [2,]  2.5   -2

## > a$set(matrix(11:14,2,2))
## > cacheSolve(a)
## [,1] [,2]
## [1,]   -7  6.5
## [2,]    6 -5.5

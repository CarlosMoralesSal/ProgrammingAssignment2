## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##The following function makeCacheMatrix creates a special "matrix", which is really a list containing a function to
##Set the value of the matrix
##Get the value of the matrix
##Set the inverse of the matrix
##Get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  z <- NULL
  set <- function(t)
  {
    x <<- t
    z <<- NULL
  }
  get <- function() x
  putinverse <- function(inverse) z <<- inverse
  getinverse <- function() z
  list(set = set, get = get, putinverse = putinverse, getinverse = getinverse)
}


## Write a short comment describing this function

##The following function cacheSolve calculates the inverse
##of the special "matrix" created with the above function 
##it first checks to see if the inverse of the matrix 
##has already been calculated. If so, it gets the inverse 
##from the cache and skips the computation. Otherwise, 
##it calculates the inverse of the data and sets the 
##inverse matrix in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  z <- x$getinverse()
  if(!is.null(z)) {
    message("getting cached data")
    return(z)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$putinverse(z)
  z
}

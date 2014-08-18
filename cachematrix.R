# This script contains two functions to create, cache and access
# the inverse of a matrix
#======
# Author: Austin Haffenden
# 18Aug14
#===================================

# This function creates a special "matrix" object 
# that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {

  # im is the inverse matrix and is reset to NULL when makeCacheMatrix
  # is called
  im <- NULL              
  #=============
  # This functin not run when cacheMatrix called but 
  # could be used for debugging etc. 
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  #==============  
  # Following three functions aren't called when makeCacheMatrix is called
  # instead, they are used by cacheSolve() to get values for x or for
  # the inverse matrix (im) and for solving the inverse matrix.  
  # These are usually called object methods
  
  # this returns the value of the original vector
  get <- function() x
  
  # this is called by cachemean during the first cachemean() call
  # and it is stored using superassignment 
  setsolve <- function(solve) im <<- solve
  
  # this returns the cached value to cachemean if available
  getsolve <- function() im
  
  # this allows subfunctions to be accessed. 
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
    
}


# This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has already
# been calculated (and the matrix has not changed), then 
# cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  # accesses the object 'x' and gets the value of the mean
  im <- x$getsolve() 
  
  # if the solve is already cached (not NULL)
  # it is returned
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  
  # we only reach this code if 'im' is Null 
  # this gets the original data
  data <- x$get()
  
  # this calculates the solve of the matrix
  im <- solve(data, ...)
  
  # and stores the calculated mean in x (setmean() in makeVector)
  x$setsolve(im)
  
  # the mean is then returned to the calling env. 
  im
  
  
}

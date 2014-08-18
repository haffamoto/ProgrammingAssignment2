makeVector <- function(x = numeric()) {
  
  # m is mean and is reset to NULL when makeVector
  # is called
  m <- NULL              
#=============
  # This functin not run when cachemean called but 
  # could be used for debugging etc. 
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
#==============  
# Following three functions aren't called when makevector is called
# instead, they are used by cachemean() to get values for x or for
# the mean (m) and for setting the mean. 
# These are usually called object methods

  # this returns the value of the original vector
  get <- function() x

  # this is called by cachemean during the first cachemean() call
  # and it is stored using superassignment 
  setmean <- function(mean) m <<- mean

  # this returns the cached value to cachemean if available
  getmean <- function() m

  # this allows subfunctions to be accessed. 
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}


# input is an object created by makeVector
cachemean <- function(x, ...) {
  
  # accesses the object 'x' and gets the value of the mean
  m <- x$getmean() 
  
  # if the mean is already cached (not NULL)
  # it is returned
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # we only reach this code if 'm' is Null 
  # this gets the original data
  data <- x$get()
  
  # this calculates the mean
  m <- mean(data, ...)
  
  # and stores the calculated mean in x (setmean() in makeVector)
  x$setmean(m)
  
  # the mean is then returned to the calling env. 
  m
}
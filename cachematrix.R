####################################
# This file has two functions      #
# to calculate and store matrix    #
# and its inverse                  #
####################################

#-----------------------------------------------------------------------
## This is the first function, that 
## creates and stores 4 getter/setter methods
## for the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL                         # initialization of the inverse
  set <- function(y) {              # matrix setter
    x <<- y                         # set matrix in global env
    i <<- NULL                      # set inverse as NULL in global env
  }
  get <- function() x               # matrix getter
  setinv <- function(inv) i <<- inv # inverse matrix setter in global env
  getinv <- function() i            # inverse matrix getter
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)             # form a list with all methods
}


#-----------------------------------------------------------------------
## This is the second function, that
## calculates inverse of the matrix
## if not already calculated and
## stores it in the cache for the
## future use. If inverse is 
## already calculated then it returns
## stored value instead of recalculating
cacheSolve <- function(x, ...) {
  i <- x$getinv()                  # get cached inverse or NULL
  if(!is.null(i)) {                # if not NULL use it
    message("getting cached data")
    i
    return(i)
  }
                                   # stored inverse is NULL
  data <- x$get()                  # get the matrix
  i <- solve(data)                 # calculate its inverse
  x$setinv(i)                      # set inverse in global env
  message('calculating and setting data to the cache')
  i
}

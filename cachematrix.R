## General Desciption of Code ##

#This pair of functions calculates and caches the inverse of a matrix (if the inverse exists).

# The first function named makeCacheMatrix creates a special "matrix" object that can cach
# the inverse of a given matrix.  

#The second function named cacheSolve calculates the inverse matrix
# of the "matrix" object created with the makeCacheMatrix function (or pulls it from the cache if it
# has previously been calculated.

######################################################################################################
## Detail description and code ##

## This function creates a special "matrix" object that can cache its inverse.
# It is really a list containing a function to:
#1. define the matrix
#2. get the matrix as defined
#3. define the inverse of the original matrix
#4. get the inverse of the original matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m ## Return a matrix that is the inverse of 'x'
}



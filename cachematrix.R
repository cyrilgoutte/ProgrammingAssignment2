## Put comments here that give an overall description of what your
## functions do


# These two functions mimic how the vector/mean example works, but
# provide matrix + inverse.
#
# I. "makeCacheMatrix" sets up the "special matrix" object, which
#    is in effect a list of four functions, plus their implicit
#    environment.
#    The actual matrix and its inverse are in the "environment", and
#    may be accessed and modified through the 4 functions.
#
# II. "cacheSolve" leverages this to provide the matrix inverse,
#    given a "special matrix" object.
#    If the inverse already exists in the cache, it is retrieved.
#    Otherwise, the inverse is computed and cached for future use.


## Write a short comment describing this function
#
# This is the instanciation of the matrix-with-cached-inverse object.
# This mimics the example of the vector with cached mean in the
# assignment description.
#
# This creates a list of 4 functions allowing to
# 1) access (get) and set the matrix
# 2) access (getinv) and set (setinv) the value of the inverse 
# !!: There is NO guarantee that the matrix and its inverse are
#     consistent. Using the methods in the object it's possible
#     to set both (almost) independently. That's a feature of
#     the assignment, though.
#
# Input: matrix
# Output: special matrix object
#
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL ;
  # Create (set) object
  set <- function(mtrx) {
    x <<- mtrx ;
    s <<- NULL ;
  }
  # Get matrix
  get <- function() {
    x ;
  }
  # Set value of inverse matrix (x^-1) in object
  setinv <- function(minv) {
    s <<- minv ;
  }
  # Get the inverse x^-1 from object
  getinv <- function() {
    s ;
  }
  # Return object as list of 4 functions:
  list(set = set, get = get, setinv = setinv, getinv = getinv) ;
}


## Write a short comment describing this function
#
# This function provides the inverse of the "special matrix". The inverse
# is either retrieved from the cache, if available, or it is computed and
# placed in the cache, before being returned.
#
# Input: special matrix object
# Output: inverse of the matrix
#
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  # Get content of cache
  xinv <- x$getinv() ;

  # If cache not empty, return cached inverse
  if(!is.null(xinv)) {
    message("getting cached data")
    return(xinv) ;
  }

  # Otherwise... get matrix from object, invert it, cache inverse and return it. 
  mtrx <- x$get() ;
  xinv <- solve(mtrx, ...) ;
  x$setinv(xinv) ;
  xinv ;
}

## This is a short test. Running it should print out
# - The 5x5 matrix M
# - "NULL" indicating the inverse does not exist yet
# - The inverse, through the "cacheSolve" function
# - The inverse again, but through the getinv() method of the object
test <- function() {
  M <- matrix(c(4,3,2,1,0,3,4,3,2,1,2,3,4,3,2,1,2,3,4,3,0,1,2,3,4), nrow=5, ncol=5) ;
  
  MM <- makeCacheMatrix(M) ;
  
  print(MM$get()) ;

  print(MM$getinv()) ;
  
  print(cacheSolve(MM)) ;
  
  print(MM$getinv()) ;
}
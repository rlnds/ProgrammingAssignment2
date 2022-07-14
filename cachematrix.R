## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# The first function, makeCacheMatrix creates a list (special "matrix") 
# containing a function to:
#   set the matrix;
#   get the matrix;
#   set the inverse matrix;
#   get the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL 
  set <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  get <- function() x
  setInverse <- function(inverseMatrix) invMatrix <<- inverseMatrix
  getInverse <- function() invMatrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Write a short comment describing this function

# The second function, cacheSolve calculates the inverse of the special "matrix"
# created with the first function. It first checks to see if the inverse has 
# already been calculated. If so, it gets the inverse from the cache and skips
# the computation. Otherwise, it calculates the inverse of the matrix and sets
# the value of the inverse matrix in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invMatrix <- x$getInverse()
  if(!is.null(invMatrix)) {
    message("getting cached data")
    return(invMatrix)
  }
  data <- x$get()
  invMatrix <- solve(data, ...)
  x$setInverse(invMatrix)
  invMatrix
}

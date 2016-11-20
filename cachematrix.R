# makeCacheMatrix creates a "matrix" that can cache its inverse. 
# it has 4 functions
# (1) get returns the Matrix
# (2) set changes the Matrix
# (3) setinverse and (4) getinverse set and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


# cacheSolve computes the inverse of the special “matrix” passed as input
# The special matrix is created by makeCacheMatrix
# If the inverse is already cached, then cacheSolve should return the inverse from the cache. 
# If not, calculates the inverse and stores it in the object m in makeCacheMatrix.

cacheSolve <- function(x, ...) {

  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached Matrix data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

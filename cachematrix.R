## Put comments here that give an overall description of what your
## functions do

## This is a function to cache potentially time consuming computations. 
## makeCacheMatrix creates a matrix object to cache an inputted matrix 
## its inverse. 

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

## calls functions from makeCacheMatrix so that if the inverse of a matrix
## has already been calculated, then the inverse is obtained from the cache,
## otherwise, the inverse is valculated and stored in a cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$getinverse()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

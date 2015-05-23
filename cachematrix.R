## These 2 functions cache potentially time consuming computations. 
## make_matrix creates a matrix object to cache an inputted matrix and its inverse. 
## invert_matrix calculates the inverse of an object, first retrieving it from the cache if possible.

## Function 1 of 2 --------------------------------------------------------------------------------------------------------------

make_matrix <- function(x = matrix()) {
  ## creates a special "matrix", which is really a list containing four functions
  ##
  ## args:  
  ##      x: a matrix
  ##
  ## Returns: 
  ## 4 functions to:  
  ## 1. set the value of the matrix - set
  ## 2. get the value of the matrix - get
  ## 3. set the value of the matrix - setmean
  ## 4. get the value of the mean - getmean 
  
  inv <- NULL  ## restores to null the value of the inv (inverse of the old matrix no longer needed)
  
  set <- function(y) {
    ## set - function that changes the matrix stored in the main function.
    x <<- y
    inv <<- NULL
  }
  
  get <- function() {
    ## returns the matrix x stored in the main function, doesn't require any input.
    x
  }
  
  setinverse <- function(inverted_matrix) {
    ## store the value of the input (inverted_matrix) in a variable inv into the main function
    inv <<- inverted_matrix                               
  }
  getinverse <- function() {  
    ## return value of the matrix inv into the main function
    inv                                                   
  }
  ## store 4 functions created
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) 
}


## Function 2 of 2 ----------------------------------------------------------------------------------------------------------------
invert_matrix <- function(x, ...) {
  ## calculates the mean of the special "matrix" created with make_matrix. 
  ## first checks to see if the inverse has already been calculated, and if so gets the inverse from cache and skips the calculation
  ## otherwise, it calculates the inverse and sets the value of the inverted_matrix in the cache via the setinverse funtion.
  ##
  ## Args: 
  ##      x: the object where make_matrix is stored
  ## 
  ## Returns:
  ##    the inverse of the matrix x
  
  inv <- x$getinverse()
  if(!is.null(inv)) {           # if the mean was cached
    message("getting cached data")
    return(inv)                 # exit without executing subsequent code
  }
  data <- x$get()             # otherwise, put the data in 'data'
  inv <- solve(data, ...)        # compute the mean of data
  x$setinverse(inv)                # cache the mean, return the mean on the next line
  inv
} 
## Assignment: Caching the inverse of a matrix
## A pair of functions that calculate/cache the inverse of a matrix

## This function creates a special “matrix” object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  ## creates a special "matrix", 
  ## which is really a list containing a function to
  ##
  ## set the value of the matrix
  ## get the value of the matrix
  ## set the value of the inverse
  ## get the value of the inverse
  
  inputMatrix <- x
  cachedMatrix <- NULL
  
  set <- function(newMatrix) {
    inputMatrix <<- newMatrix
    cachedMatrix <<- NULL
  }
  
  get <- function() inputMatrix
  
  setinverse <- function(inverse) {
    cachedMatrix <<- inverse
  }
  
  getinverse <- function() cachedMatrix
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special “matrix” returned 
## by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("cached data")
    return(inverse)
  }
  else {
    data <- x$get()
    inverse <- solve(data)
    x$setinverse(inverse)
    return(inverse)
  }
}

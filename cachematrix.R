## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: Create a list object that create a special matrix that can
## store its inverse in a cache

makeCacheMatrix <- function(x = matrix()) {
  ## Set inverse to NULL by default
  minv <- NULL
  
  ## Function to populate the matrix using argument value
  set <- function(y) {
    x <<- y
    minv <<- NULL
  }
  
  ## Function to retrive the value of matrix
  get <- function() x
  
  ## Function to ache the argument value as matrix inverse
  setinv <- function(mi) minv <<- mi
  
  ## Function to retrive cached matrix inverse
  getinv <- function() minv
  
  ## Return the list object
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve: Check if inverse of the matrixi is already cached and return it
## if not, cache it and return

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## Retrive cached matrix inverse
  minv <- x$getinv()
  
  ## Check if matrix inverse isn't NULL and return the value
  if(!is.null(minv)) {
    message("Getting a cached inverse")
    return(minv)
  }
  
  ## Retrive the matrix
  m <- x$get()
  
  ## Compute the matrix inverse
  minv <- solve(m)
  
  ## Cache the matrix inverse
  x$setinv(minv)
  
  ## Return the matrix inverse
  minv
}

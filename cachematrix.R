## The following functions are responsible to cache the inverse of a matrix
## so that if the value of the matrix has not changed then we can look up
## the inverse of the matrix from cache rather than recomputing.
## 
## author: vimal
## date: 7/25/2015

## This function "makeCacheMatrix" creates a special "matrix" object that 
## can cache its inverse.
## The inverse of the matrix is calculated with the "solve" function in R.
## It is assumed that the matrix supplied is always invertible.
## This function creates and returns a list which contains a function to:
## (1) set the value of the matrix
## (2) get the value of the matrix
## (3) set the value of inverse of the supplied matrix
## (4) get the value of inverse of the supplied matrix

makeCacheMatrix <- function(x = matrix()) {
  inversed_matrix <- NULL
  
  ## set the value of the matrix
  set <- function(y) {
    x <<- y
    inversed_matrix <<- NULL
  }
  
  ## get the value of the matrix
  get <- function() x
  
  ## set the value of inverse of the supplied matrix
  setinverse <- function(solve) inversed_matrix <<- solve

  ## get the value of inverse of the supplied matrix
  getinverse <- function() inversed_matrix
  
  ## create and return the list of getter and setter functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function "cacheSolve" calculates the inverse of the supplied matrix 
## created with the function "makeCacheMatrix".
## It first checks to see if the inverse has already been calculated. If so, 
## it gets the inverse from the cache and skips the computation.
## Otherwise it calculates the inverse of the matrix and sets the value of
## inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'

  inversed_matrix <- x$getinverse()
  
  ## Check to see if the inverse is present cache
  if(!is.null(inversed_matrix)) {
    ## Inverse is present in cache, so get the cached inverse value
    message("getting cached data...")
    
    ## Return the cached inverse and exit from the function
    return(inversed_matrix)
  } else {
    ## Inverse is not present in  cache, so proceed to calculate inverse
    message("no data cached...calculating for first time...")  
  }
  
  data <- x$get()
  
  ## Calculate the inverse
  inversed_matrix <- solve(data, ...)
  
  ## Cache the calculated inverse
  x$setinverse(inversed_matrix)
  
  ## Return the inverse value
  inversed_matrix  
}

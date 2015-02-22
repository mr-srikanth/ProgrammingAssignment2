## Matrix inversion is usually a costly operation
## With this implementation of cached Matrix inversion, we optimize the process
## of repeatedly computing inverse of the same matrix.
## We do so by storing the computed result, and return cached results when inverse
## is requested for same matrix


## This method creates a cached version of our matrix. 
## input: x is a matrix
## output: 'cached Matrix' object, that allows cached computations on matrix

makeCacheMatrix <- function(cachedMatrix = matrix()) {
  # This variable holds the computed (cached) inverse
  cachedInverse <- NULL
  
  setMatrix <- function(newMatrix) {
    message("trying to set ")
    ## We only need to change the inverse if the matrix has changed.
    ## If old and new matrices are identical, keep the computed inverse as it is
    if(identical(newMatrix, cachedMatrix) == FALSE){
      message("Storing a new matrix")
      cachedMatrix <<- newMatrix
      cachedInverse <<- NULL
    }
    
  }
  
  ## getMatrix returns the input matrix
  getMatrix <- function() cachedMatrix
  
  ## setInverse is used to store the newly computed inverse to cache
  setInverse <- function(inverseMatrix) cachedInverse <<- inverseMatrix
  
  ## getInverse  returns the cached Inverse result
  getInverse<- function() cachedInverse
  
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse, getInverse = getInverse)
  
}


## cacheSolve computes the Inverse of a matrix
## 1. It computes the inverse if the inverse was not already computed
## 2. It returns cached Inverse result, if it was computed already.
## Input: x is a cached matrix object, created by using 'makeCacheMatrix' method
cacheSolve <- function(x, ...) {
  result <- x$getInverse()
  if(!is.null(result)){
    ## result is not null, which means the inverse was computed earlier
    ## We can return the cached result
    return(result)
  }
  
  ## If we have reached this point, we should compute the inverse
  inverse <- solve(x$getMatrix())
  
  ## save the computed result for future reference
  x$setInverse(inverse)
  inverse
}

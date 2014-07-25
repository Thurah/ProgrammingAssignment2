## cache the inverse of a matrix, so only to calculate it when needed

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverseMatrix) inverse <<- inverseMatrix
  getInverse <- function() inverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  theMatrix <- x$get()
  inverseMatrix <- solve(theMatrix)
  x$setInverse(inverseMatrix)
  inverseMatrix
        ## Return a matrix that is the inverse of 'x'
}

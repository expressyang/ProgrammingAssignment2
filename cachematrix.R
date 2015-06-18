## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function



makeCacheMatrix <- function(x = matrix()) {
  # initialize the value of the matrix inverse to NULL
  matrixinverse <- NULL
  # declare another function set where the value will be cached in 1. Matrix is created
  # for the first time. 2. changes made to cached matrix
  set <- function(y) {
    x <<- y
    # change the value of inverse of the matrix in case the matrix was changed.
    matrixinverse <<- NULL
  }
  # gets the value of the inverse
  get <- function() x
  #calculates the inverse of non-singular matrix via the solve function
  setInverseMatrix <- function(solve) matrixinverse <<- solve
  # gets the inverse
  getInverseMatrix <- function() matrixinverse
  # passes the value of the function makeCacheMatrix    
  list(set = set, get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)

}




## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  matrixinverse <- x$getInverseMatrix()
  #if the inverse exists, it gets it.
  if(!is.null(matrixinverse)) {
    message("getting cached data")
    return(matrixinverse)
  }
  #if the inverse if not there, first it is calculated and then retrieved.
  data <- x$getInverseMatrix()
  matrixinverse <- solve(data, ...)
  x$setInverseMatrix(matrixinverse)
  matrixinverse
}

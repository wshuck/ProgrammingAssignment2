## Put comments here that give an overall description of what your
## functions do

## The following two functions cache the inverse of a matrix.
## This function creaes a matrix object that can cache its inverse.

makeCacheMatrix <- function(m = matrix()) {
      
      ## Initializes the inverse property
      i <- NULL
      
      ## Set the matrix
      setMatrix <- function(matrix) {
            m <<- matrix
            i <<- NULL
      }
      
      ## Get the matrix
      getMatrix <- function() {
            m
      }
      
      ## Sets the inverse of the matrix
      setInverseMatrix <- function(inverse) {
            i <<- inverse
      }
      
      ## Get the inverse of the matrix
      getInverseMatrix <- function() {
            i
      }
      
      ## Return a list of the methods
      list(setMatrix = setMatrix, getMatrix = getMatrix,
            setInverseMatrix = setInverseMatrix,
            getInverseMatrix = getInverseMatrix)

}


## This function computes the inverse of the special matrix returned by 
## "makeCacheMatrix" above, If the inverse has already been calculated 
## (and the matrix has not changed), then the "cacheSolve" should retrieve
## the inverse from the cache.

cacheSolve <- function(x, ...) {
      
      ## Return a matrix that is the inverse of 'x'
      m <- x$getInverseMatrix()
      
      ## Return the inverse if its already set
      if (!is.null(m)) {
            message("Getting cached data")
            return(m)
      }
      
      ## Get the matrix from the object
      data <- x$getMatrix()
      
      ## Calculate the inverse using matrix multiplication
      m <- solve(data) %*% data
      
      ## Set the inverse to the object
      x$setInverseMatrix(m)
      
      ## Return the matrix
      m
}

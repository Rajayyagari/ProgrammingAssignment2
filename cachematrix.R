## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(inputMatrix = matrix()) {
  cacheInvMatrix <- NULL
  set <- function (y)
  {
    inputMatrix <<- y
    cacheInvMatrix <<- NULL
  }
  get <- function () inputMatrix
  setInvMatrix <- function (invMatrixResult) cacheInvMatrix <<- invMatrixResult
  getInvMatrix <- function () cacheInvMatrix
  list (set = set, get = get, setInvMatrix = setInvMatrix, getInvMatrix = getInvMatrix)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  print (environment())
  ## Return a matrix that is the inverse of 'x'
  invertedMatrix <- x$getInvMatrix()
  if (!is.null(invertedMatrix))
    {
      message("Inverted Matrix from Cache")
      return (invertedMatrix)
    }
  invertedMatrix <- solve(x$get(),...)
  x$setInvMatrix(invertedMatrix)
  invertedMatrix

}

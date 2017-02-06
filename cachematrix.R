## makeVectorMatrix - creates functions and environment needed to cache the results 
## cacheSolve - Solve is function to invert the matrix. cacheSolve 
## executes the functions created during execution of makeVectorMatrix
## and if inverted matrix exists in cache will get it from there. 

## The following is the order of execution of my functions 

##  source("cachematrix.R")
##  CREATE THE VECTOR and SAVE THE INPUT
##  mvM <- makeVectorMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))

## INVERT THE MATRIX
##  cacheSolve(mvM)
##       [,1] [,2]
##[1,]   -2  1.5
## [2,]    1 -0.5

## SECOND EXECUTION SAYS IT GOT THE RESULT FROM THE CACHE
## rslt <- cacheSolve(mvM)
##  Inverted Matrix from Cache

##CHECK THE RESULT IS ACTUALLY THE INVERTED MATRIX
## rslt %*% (matrix(c(1,2,3,4), nrow=2, ncol=2))
##        [,1] [,2]
## [1,]    1    0
## [2,]    0    1
 
##############

## Do not run cacheSolve(makeVectorMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
## You will the result but the data is not cached as vector getting memory every time
## You can check this by printing environment()  in makeVectorMatrix
  
makeVectorMatrix <- function(inputMatrix = matrix()) {
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

  ## Return a matrix that is the inverse of 'x'

  ##if inverted matrix is available in cache return it.
  invertedMatrix <- x$getInvMatrix()
  if (!is.null(invertedMatrix))
    {
      message("Inverted Matrix from Cache")
      return (invertedMatrix)
  }
  ##invert the input
  invertedMatrix <- solve(x$get(),...)
  ## cache the result into x
  x$setInvMatrix(invertedMatrix)
  ## Return the result
  invertedMatrix
}

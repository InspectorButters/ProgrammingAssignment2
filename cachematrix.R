
##Overview:  The purpose of this code is to (1) calculate the inverse of a matrix and
## (2) to cache and recall this inverse matrix.

##set value of myMatrix
myMatrix <- matrix(c(1,2,3,5,7,11,13,17,19), 3, 3)

##this function inverts myMatrix
myMatrixInvert <- function(myMatrix){
  solve(myMatrix)
}


##the following function is supposed to cache the matrix
##inputted above in myMatrix
makeCacheMatrix <- function(x = myMatrix()) {
  m <- NULL
  setMatrix <- function(y) {
    x <<- y
    m <<- NULL
  }
  getMatrix <- function() x
  setInverse <- function(solve(x)) m <<- solve(x)
  getInverse <- function() m
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}


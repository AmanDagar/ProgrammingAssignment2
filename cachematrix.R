## Put comments here that give an overall description of what your
## functions do
## First function creates special object that can store inverse
## of passed matrix as cache in another environment while the
## second function calculate or retrieve cached mean of matrix
## using first function
## Write a short comment describing this function
## The makeCacheMatrix function takes a matrix as input and returns
## a list of 4 functions : 2 to get and set inverse,1 to get x
## and 1 to set x(the passed matrix) and set inverse of x to NULL
## later this inverse is updated from cacheSolve where inverse is
## calculated
makeCacheMatrix <- function(x = matrix()) {
      inverse <- NULL
      setMatrix <- function(y){
          x <<- y
          inverse <<- NULL
      }
      getMatrix <- function() x
      setInverse <- function(inv) inverse <<- inv
      getInverse <- function() inverse
      list(setMatrix=setMatrix,getMatrix=getMatrix,
           setInverse=setInverse,getInverse=getInverse)
}


## Write a short comment describing this function.
## This function check if inverse is already cached and retrieves it
## If inverse is not cached then it calculates it and then cache it 
## using first function and also returns the inverse as output
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    if(!is.null(inverse)){
      message("Getting cached Inverse :D")
      return(inverse)
    }
      data <- x$getMatrix()
      inverse <- solve(data,...)
      x$setInverse(inverse)
      inverse
}

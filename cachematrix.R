############ Programming Assignment 2: Lexical Scoping ############

ls()
rm(list=ls())
ls()

# 1: This function creates a special "matrix" object that can cache its inverse:

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  #will hold value of matrix inv
  set <- function(y) {  # define the set function
    x <<- y               #use "<<-" to assign a value to an object from an environment different from the current one. This is the value of the matrix.
    inv <<- NULL  # if there is a new matrix, reset inv to NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse #to calculate the inverse
  getInverse <- function() inv  #gets the value of inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


# 2: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache:

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  
  ## if the inverse has already been calculated
  if (!is.null(inv)) {
    message("Getting cached data...")
    return(inv)
  }
  
  #if the inverse has not been calculated, it calculates it:
  mydata <- x$get()
  inv <- solve(mydata, ...)
  x$setInverse(inv)
  inv
}


## TESTING... ##
mimatriz <- makeCacheMatrix(matrix(1:4, ncol=2, nrow=2))
mimatriz$get()
mimatriz$getInverse()
cacheSolve(mimatriz)
cacheSolve(mimatriz)

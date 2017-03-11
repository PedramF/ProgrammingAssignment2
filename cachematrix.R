## This solution is to calculate the inverse of a Matrix
## if a Matrix is large, this can be costly to the performance
## Thus, this code checks if the inverse have previously been calculated instead of re-calculating it repeatedly
  ## If Yes return inverse
  ## If No calculate inverse


## makeCacheMatrix creates a list with a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse 
## 4. get the value of inverse 

##ASSUMPTION: makeCacheMatrix takes x <- a Square invertible Matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    
    #`<<-` enables assigning a value to an object in an different environment than in current 
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse 
  getInverse <- function() inv
  
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

## The following function returns the inverse of the matrix. 
## It checks if the inverse has already been computed. 
  ## If Yes, returns inverse
  ## if No, computes the inverse, sets the value in the cache with the setInverse function.

## CONDITION: cacheSolve takes x <- a Square invertible Matrix, previously run in makeCacheMatrix

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  
  # if statment Yes or NO
  # if Yes - Code stops at Return in if
  if (!is.null(inv)){
  
    message("getting cached data")
    return(inv)
  }
  
  # if No 
  data <- x$get()
  inv  <- solve(data, ...)
  
  # sets the value of the inverse in the cache via the setInverse function.
  x$setInverse(inv)
  
  return(inv)
}

##Try this code by:
## x <- matrix(c(1, -1/4, -1/4, 1), nrow = 2, ncol = 2)
## m <- makeCacheMatrix(x)
## m$get()
##       [,1]  [,2]
## [1,]  1.00 -0.25
## [2,] -0.25  1.00

## No cache in the first run
## > cacheSolve(m)
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667

## Retrieving from the cache in the second run
## > cacheSolve(m)
## getting cached data.   NOTICE THIS LINE ON THE SECOND RUN
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
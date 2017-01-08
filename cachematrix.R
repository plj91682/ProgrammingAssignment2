## This assignment contains 2 functions written in the R-language
## A summary of them follow:
##
## makeCacheMatrix: accept a matrix parameter and cache it
##                  with the <<- operator
##
## cacheSolve: returns the inverse of the passed in matrix
## 
## To test these functions run the below example:
##
##   > m1 = matrix(c(4,5,3,9,8,700,1,3,2),3,3)
##   > cacheSolve(makeCacheMatrix(m1))
##
## The result will be:
##  [,1]                     [,2]         [,3]
##  [1,]  0.428013966 -0.140069830 -0.003902239
##  [2,]  0.000205381 -0.001026905  0.001437667
##  [3,] -0.713904292  0.569521462  0.002669953


## This function should use the <<- operator to cache the inverse
## of a matrix
makeCacheMatrix <- function(x = matrix()) {
  inverse_of_matrix = NULL
  set <- function(y) {
    x <<- y
    inverse_of_matrix <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) inverse_of_matrix <<- solve
  getInverse <- function() inverse_of_matrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function will check the cache for an inverse matrix
## If it exists, it will will return the value
## If it doesn't, exist it will compute the inverse and then return it
cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
  
}

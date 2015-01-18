## Function makeCacheMatrix creates a list of functions to set and get the value of a matrix and its inverse.
## Function cacheSolve returns the cached inverse of matrix defined in an object created by makeCacheMatrix.


## This function receives a matrix as input parameter. 
## It creates a list of functions to set the value of the matrix, to get the value of the matrix, 
## to get the value of the inverse of the matrix, and to set the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinv <- function(invmatrix) inv <<- invmatrix
      getinv <- function() inv
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)

}


## This function returns the inverse of the matrix created by function makeCacheMatrix. 
## First uses the function getinv defined by makeCacheMatrix to get the cached inverse.
## If the inverse has not been calculated yet (it has not been cached yet) 
## uses the function get to get the matrix created by makeCacheMatrix, calculates the inverse,
## uses the function setinv to save the inverse in the cache, and returns the inverse.
# If the inverse has already been calculated before, it returns its value.
cacheSolve <- function(x, ...) {
      inv <- x$getinv()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data)
      x$setinv(inv)
      inv
}

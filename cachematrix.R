## These functions creates a object that stores a matrix and caches its inverse 
## if the matrix is new or has not been changed. 


## This function creates a speical matrix object that  
## sets the value of maxtrix, 
## gets value of matrix, 
## sets the vale of inverse 
## and gets the value the inverse.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the matrix returned by the makeCacheMatrix. 
## If the inverse has already been calculated and the matrix has not changed, then the inverse is retrieved from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}

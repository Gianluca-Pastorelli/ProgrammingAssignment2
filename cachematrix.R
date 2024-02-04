### These functions cache and retrieve/compute the inverse of a matrix.


## Create a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize the inverse as NULL (not calculated yet)
  
  # Set the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset the inverse if the matrix changes
  }
  
  # Get the value of the matrix
  get <- function() x
  
  # Set the value of the inverse
  setinverse <- function(inverse) inv <<- inverse
  
  # Get the value of the inverse
  getinverse <- function() inv
  
  # Return a list containing functions to get/set matrix and inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Compute the inverse of the matrix returned by makeCacheMatrix and stored as a list.
## Retrieve the inverse from the cache if it has already been calculated.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)  # Calculate the inverse using solve()
  x$setinverse(inv)  # Cache the inverse
  inv
}

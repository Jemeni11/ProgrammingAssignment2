## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize the cached inverse to NULL
  
  # Setter function - This sets the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Invalidate the cached inverse when the matrix is changed
  }
  
  # Getter function - This returns the value of the matrix
  get <- function() {
    x
  }
  
  # Inverse setter function - This sets the cached inverse
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  
  # Inverse getter function - This returns the cached inverse
  getInverse <- function() {
    inv
  }
  
  # Return a list of the above functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()  # Call the getter to return the cached inverse matrix
  if (!is.null(inv)) {  # If the cached inverse is not null, return it
    message("Getting cached matrix")
    return(inv)
  }
  data <- x$get()  # Call the getter to return the matrix
  inv <- solve(data, ...)  # Compute the inverse
  x$setInverse(inv)  # Cache the inverse
  inv  # Return the inverse
}

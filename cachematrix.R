## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  # A function that sets the value for the matrix
  set <- function(y) { 
    x <<- y
    m <<- NULL
  }
  # A function that gets value predefined before
  get <- function() x
  # A function that sets the inverse of the matrix
  setinv <- function(inv) m <<- inv
  # A function that gets the inverse of the matrix
  getinv <- function() m
  # Output returns a list of functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  # The below expression gets the inverse of the matrix
  m <- x$getinv()
  # Conditional checking whether if m is predefined or not
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # If m is null then the following expressions gets data
  data <- x$get()
  # The following expression computes the inverse of the matrix
  m <- solve(data, ...)
  # The following expression sets the inverse computed
  x$setinv(m)
  m
}

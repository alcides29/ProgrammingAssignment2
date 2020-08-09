## Caching the inverse of a matrix
##
## Creates a list containing the getter/setter functions to acces the variables
## `x` (the matrix) and `inv` (inverse of the matrix). In fact, these variables
## are not stored inside the list but somewhere in the memory and the functions
## of the list directly access to the address which the variables occupy.
##
## If the data `x` is changed, the inverse is NULLed (the cache gets cleared).

makeCacheMatrix <- function(x = matrix()) {
  # initialize the inverse matrix of x with NULL
  inv <- NULL
  
  # set function: assign mat to x and NULL to inv
  set <- function(mat) {
    x <<- mat
    inv <<- NULL
  }
  
  # get function: return x
  get <- function() x
  
  # method to set the inverse of a matrix
  setinv <- function(inverse) inv <<- inverse
  
  # method to get the inverse of a matrix
  getinv <- function() inv
  
  # return a list of methods
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Calculate the inverse of `x`. If the inverse is NULL (the cache has been
## cleared)
## the inverse is calculated using the `solve()` function. Otherwise the cached
## value is used to save the computational time.

cacheSolve <- function(x, ...) {
  # return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  
  # if the inverse was already computed, return the existing one
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
    
  # get the matrix from our object
  data <- x$get()
    
  # calculate the inverse and then set the cache.
  # solve(a, b, ...) function solves the equation a %*% x = b for x,
  # where b can be either a vector or a matrix.
  inv <- solve(data, ...)
    
  # set the inverse
  x$setinv(inv)
    
  # return the inverse
  inv
}

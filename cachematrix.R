## This function creates a special "matrix" object that can cache its inverse

# the input of the function should be a matrix
makeCacheMatrix <- function(x = matrix()) {
  # define inverse
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  ## put 'x' in get
  get <- function() x
  ## calculate the inverse (solve function) and assign the result to the name "inverse"
  setInverse <- function(solve) inverse <<- solve
  ## print the result of setInverse
  getInverse <- function() inverse
  ## create the list that will be used by cachesolve function (as the return value is the last line of code)
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## 
  inverse <- x$getInverse()
  ## Print message if inverse has already been calculated, then return value
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  ## calulate the inverse of matrix 'x' if not already been calculated
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
}

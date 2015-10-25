## Put comments here that give an overall description of what your
## functions do

## Create a matrix object that is able to store and retrieve the matrix and its inversion

makeCacheMatrix <- function(x = matrix()) {
  inversedMatrix <- NULL
  set <- function(y) {
    x <<- y
    inversedMatrix <<- NULL
  }
  get <- function() x
  setInversion <- function(inversion) inversedMatrix <<- inversion
  getInversion <- function() inversedMatrix
  list(set = set, get = get,
       setInversion = setInversion,
       getInversion = getInversion)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inversedMatrix <- x$getInversion()
  ## if it is calculated, return the cached result
  if(!is.null(inversedMatrix)) {
    message("getting cached data")
    return(inversedMatrix)
  }
  data <- x$get()
  inversedMatrix <- solve(data, ...)
  x$setInversion(inversedMatrix)
  inversedMatrix
}

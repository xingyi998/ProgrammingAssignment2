## Put comments here that give an overall description of what your
## functions do

## create a matrix object that is able to store and retrieve the matrix and its inversion

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


## if the inversion of the matrix object x is already solved, return the cached inversion
## otherwise, calculate the matrix inversion

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inversedMatrix <- x$getInversion()
  if(!is.null(inversedMatrix)) {
    message("getting cached data")
    return(inversedMatrix)
  }
  data <- x$get()
  inversedMatrix <- solve(data, ...)
  x$setInversion(inversedMatrix)
  inversedMatrix
}

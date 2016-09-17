## Put comments here that give an overall description of what your
## functions do

##The functions were created based on the example of caching the mean of vector, insted of calculating the mean
##is is solving the given matrix

## Write a short comment describing this function
##This function creates a constraint of functions that set and get the solved matrix, and set and get a matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setSolved <- function(solvedMatrix) m <<- solvedMatrix
  getSolved <- function() m
  list(set = set, get = get,
       setSolved = setSolved,
       getSolved = getSolved)
}


## Write a short comment describing this function
##This function solve de matrix and returns it, if the matrix is already solved (m!=NULL) it returns the cached matrix(m)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getSolved()
  if(!is.null(m)) {
    message("getting cached data")
    m
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setSolved(m)
  m
}

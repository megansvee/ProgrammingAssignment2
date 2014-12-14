## These functions compute the inverse of a square matrix 
## If the matrix has already been inversed
## Then retrieve the inversed value from cache instead of recalculating
## Else cache the inverse to be used later

## This function sets the matrix in cache if it doesn't already exist 
## or gets the matrix from cache if it already exists

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(matrix) m <<- matrix
  getmatrix <- function() m
  list(set = set, get = get,
        setmatrix = setmatrix,
        getmatrix = getmatrix
      )
}


## This function inverses the passed in matrix
## or gets the cached value if it is already saved in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}

## RO RUN (e.g.):
# x <- c(1,2)
# Y <- c(3,4)
# z <- cbind(x,y)
# zz <- makeCacheMatrix(z)
# zz$get()
# cacheSolve(zz)
# cacheSolve(zz)

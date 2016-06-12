## cachematrix.R has two functions 1.makeCacheMatrix and 2.cacheSolve to solve a matrix and cache the solution

## makeCacheMatrix function supports setting and getting cached matrix
makeCacheMatrix <- function(x = matrix()) {
  invX <- NULL
  set <- function(y) {
    x <<- y
    invX <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) invX <<- inverse
  getinv <- function() invX
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## cacheSolve solves and caches the matrix solution to save time solving it next time
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invX <- x$getinv()
  if(!is.null(invX)) {
    message("getting cached data")
    return(invX)
  }
  data <- x$get()
  invX <- solve(data, ...)
  x$setinv(invX)
  invX
}





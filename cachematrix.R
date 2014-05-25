## Following set of functions can be used to retrieve a cached value of the
## inverse of a matrix, or calculate the inverse of a cached value does not exist.

## Following function takes in a matrix as an argument and generates a set of
## special functions that will be used in caching the inverse of the input matrix.
## Source this R file and run a <- makeCacheMatrix(matrix(rnorm(4),2,2)) and then
## run a$get, a$set, a$setinv and a$getinv to get a better idea. a$get returns
## the original input matrix and the rest describe the functions.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(solve) i <<- solve
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Following piece of code is used to check if a cached value of the inverse of
## a matrix exists, and if so retrieve it. Otherwise it runs the 'solve' function
## that is used to calculate the inverse of an invertible matrix.

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}

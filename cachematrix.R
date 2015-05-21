## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
   s <- NULL
   set <- function(y) {
      s <<- y
      s <<- NULL
   }
   get <- function() x
   setsolve <- function(solve) s <<- solve
   getsolve <- function() s
   list(set = set, get = get,
        setsolve = setsolve,
        getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
   s <- x$getsolve()
   if(!is.null(s)) {
      message("getting cached data")
      return(s)
   }
   data <- x$get()
   s <- solve(data, ...)
   x$setsolve(s)
   s
}

# Tests for correctness

stopifnot(
   all.equal(
      cacheSolve(makeCacheMatrix(
         matrix(c(-1, 0, -5, 3, -6, -3, -3, 5, 1), 3, 3))),
      matrix(c(3/2, -25/6, -5, 1, -8/3, -3, -1/2, 5/6, 1), 3, 3) )
)

stopifnot(
   all.equal(
      cacheSolve(makeCacheMatrix(
         matrix(c(1, 1, 0, 1), 2, 2))),
      matrix(c(1, -1, 0, 1), 2, 2) )
)

stopifnot(
   all.equal(
      cacheSolve(makeCacheMatrix(
         matrix(c(-1, 0, -5, 3, -6, -3, -3, 5, 1), 3, 3))),
      matrix(c(3/2, -25/6, -5, 1, -8/3, -3, -1/2, 5/6, 1), 3, 3) )
)

stopifnot(
   all.equal(
      cacheSolve(makeCacheMatrix(
         matrix(c(1, 1, 0, 1), 2, 2))),
      matrix(c(1, -1, 0, 1), 2, 2) )
)

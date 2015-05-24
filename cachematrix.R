## Funtion creates a special matrix, which is
## a list containing setters and getters
## for matrix object itself and/or it's inverse
makeCacheMatrix <- function(x = matrix()) {
   s <- NULL
   set <- function(y) {
      s <<- y
      s <<- NULL
      # operator << cause a search to made through parent
      # environments for an existing definition of the variable
      # being assigned. If such a variable is found (and its binding
      # is not locked) then its value is redefined,
      # otherwise assignment takes place in the global environment.
   }
   get <- function() x
   setsolve <- function(solve) s <<- solve
   getsolve <- function() s
   list(set = set, get = get,
        setsolve = setsolve,
        getsolve = getsolve)
   # return vector-type list of functions - setters and getters
}


## Below function solves the matrix created with function
## makeCacheMatrix. It first checks if the inverse
## has been already calculated, if yes, it returns the cached result,
## otherwise it calls solve() from the base package and then
## retuns the solution, caching it as well.
cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
   s <- x$getsolve()
   if(!is.null(s)) { # if the solution was cached, return it without
                     # executing the calculations
      message("getting cached data")
      return(s)
   }
   data <- x$get()  # otherwise, first put the matrix in 'data'
   s <- solve(data, ...)  # and solve the matrix with 'data' as arg
   x$setsolve(s)  # cache the solution
   s  # return the solution
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

# Interactive check

m <- matrix(c(-1, -2, 1, 1), 2,2)
x <- makeCacheMatrix(m)
x$get()

inv <- cacheSolve(x)
inv

inv <- cacheSolve(x)
# should print 'getting cached data'
inv

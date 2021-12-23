
##  The makeCacheMatrix function creates a special "matrix" object, which is a list of 4 functions
##  that can be used to:
##  1. set the value of the matrix
##  2. get the value of the matrix
##  3. set the value of the inverse
##  4. get the value of the inverse

makeCacheMatrix <- function(mat = matrix()) {
  inv <- NULL
  set <- function(m) {
    mat <<- m
    inv <<- NULL
  }
  get <- function() mat
  setInv <- function(i) inv <<- i
  getInv <- function() inv
  list(set = set,
       get = get,
       setInv = setInv,
       getInv = getInv)
}


##  The cacheSolve function calculates the inverse of the special "matrix" object created with 
##  the makeCacheMatrix function. However, it first checks to see if the inverse has already been
##  computed. If so, it gets the inverse from the cache and skips the computation. Otherwise, 
##  it computes the inverse of the matrix and sets the value of the inverse in the cache via 
##  the setInv function.

cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if (!is.null(inv)) {
    message("Getting cached inverse")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInv(inv)
  inv
}

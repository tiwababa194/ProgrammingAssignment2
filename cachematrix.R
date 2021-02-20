## When the function ends, it returns a fully formed object of type makeCacheMatrix() to be used by downstream R code
## The Function returns a list containing Four objects
## Assignment submitted by Tiwalade Omotosho

## The input matrix is assumed to be always invertible

makeCacheMatrix <- function(x = matrix()) {  
    m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## This Function computes the inverse of a square matrix

cacheinv <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}

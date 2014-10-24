## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix is a function that stores the inverse of a matrix and 
## returns a list of elements that contain the original matrix,
## the inverse matrix and functions to get and set those

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(mean) m <<- mean
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv) 
}


## cacheSolve calculates the inverse of the special "matrix" created with makeCacheMatrix. 
## However, it first checks to see if the inverse has already 
## been calculated. If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the inverse matrix in the cache 
##via the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached inverse")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}

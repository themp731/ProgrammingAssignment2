## Part of the Week 3 Homework Programming Project
## This is about Matrix Inversion

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  mat <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setMat <- function(solve) m <<-matrix
  getMat <- function() m
  list(set = set, get = get, 
       setMat = setMat,
       getMat = getMat)
 }


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getMat()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m<-solve(data, ...) %*% data
  x$getMat(m)
  m
}
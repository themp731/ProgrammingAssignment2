## Part of the Week 3 Homework Programming Project
## This is about Matrix Inversion

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  mat <- NULL
  
  #Internal function to set matrix in cache
  set <- function(y) {
    x <<- y
    mat <<- NULL
  }
  
  get <- function() x
  setInverse <- function(inverse) mat <<- inverse
  getMatrix <- function() mat
  list(set = set, get = get, 
       setInverse = setInverse,
       getMatrix = getMatrix)
 }


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  mat <- x$getMatrix()
  if(!is.null(mat)){
    message("getting cached data")
    return(mat)
  }
  data <- x$get()
 
  #The Actual Inversion of the Matrix
  mat <-solve(data)
  x$setInverse(mat)
  mat
}
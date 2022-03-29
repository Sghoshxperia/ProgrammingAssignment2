## A pair of functions that cache the inverse of a matrix.
## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  ## Initialize the inverse property
  set <- function(y){  ## Method to set the matrix
    x <<- y
    inv <<- NULL
  }
  ## Method the get the matrix
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix   ## Method to set the inverse of the matrix
  getInverse <- function() inv   ## Method to get the inverse of the matrix
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}



## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()  ## Get the matrix from our object
  inv <- solve(data)  ## Set the inverse to the object
  x$setInverse(inv)
  inv      
}
#end

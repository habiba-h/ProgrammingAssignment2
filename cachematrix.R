##  This is an implementation of two functions to calculate or retrieve 
##(if already computed and the data hasn't changed) the inverse of a matrix.

## makeCacheMatrix: 
##this function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL 
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
  
  
}


## cacheSolve: 
## this function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv_x <- x$getInverse()
  if(!is.null(inv_x)) {
    message("getting cached inverse")
    return(inv_x)
  }
  data <- x$get()
  inv_x <- solve(data, ...)
  x$setInverse(inv_x)
  inv_x
  
  
}

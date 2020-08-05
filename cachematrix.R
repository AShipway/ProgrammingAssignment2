## The function makeCacheMatrix creates a matrix vector object that can
## cache it's inverse.  The function cacheSolve computes the inverse of the
## returned by makeCacheMatrix if not available in the cache. If available
## in the cache the cached result is returned.

## This function creates a matrix vector object of four functions which access
## the matrix and the matrix inverse and set the matrix and matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse<- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This function calculates the inverse of the matrix returned by makeCacheMatrix
## if the result is not in the cache.  If the result is in the cache, the
## cached result is returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
  
}



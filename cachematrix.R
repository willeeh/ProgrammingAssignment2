## Functions that cache the inverse of a given matrix

## This function returns a list of functions that can set/get a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  
  setinverse <- function(iParam) i <<- iParam
  getinverse <- function() i
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function solves a inverse matrix and also it's cached for future calls

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached inverse matrix")
    return(m)
  }
  m <- solve(x$get(), ...)
  x$setinverse(m)
  m
}

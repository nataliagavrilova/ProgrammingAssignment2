## 'makeCacheMatrix' and 'cacheSolve' functions caches the inverse of matrix and returns its.
## If the inverse has already been calculated - result is retrieved from cache.

## 'makeCacheMatrix' caches the inverse of matrix.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) s <<- solve
  getinverse <- function() s
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## 'cacheSolve' returns inverse matrix for 'x'. If the inverse has already been calculated,
## then it retrieves result from cache.

cacheSolve <- function(x, ...) {
  s <- x$getinverse()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinverse(s)
  s
}

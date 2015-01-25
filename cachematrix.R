## This project helps to save time while matrix inversion, which is usually
## a costly computation by saving cache of previous calculations

## The first function creates a special matrix and can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}

## The second function returns the inverse of the given matrix 
## created with the above function, using cache data if possible.

cacheSolve <- function(x, ...) {

    m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
    
}

## makeCacheMatrix will cache the matrix and its calculated inverse if 
## the matrix is the same as previous input;

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

## cacheSolve will solve the inverse of a matrix if the matrix is new, 
## otherwise it will return the cached inverse value from previous 
## calculation with message "getting cached data";

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
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

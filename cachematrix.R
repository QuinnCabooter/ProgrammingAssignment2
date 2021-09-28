"Author: Quinn Cabooter
Assignment 2 of the R-Programming Course by Dr. Peng"

## Using the example code from the assignment and changing the m to s so it fits better with 'solve' and change all means to solve

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



## Write a short comment describing this function
#Changing all m's to s for consistency with the previous function. Again, changing all means to solve consistent with previous function. 

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

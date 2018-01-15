## This function will return the inverse of a matrix created here in the
## same function, to save time incase the output is needed repeatedly. The
## function is set up in two steps. The first step involves creating 
## the inverse of matrix while the second one involves calculating the 
## inverse matrix. 

## This function creats and stores anc caches its inverse a special matrix 
## through a set of functions ( set, get, getinverse, and setinverse)

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
      set <- function(y) {
              x <<- y
              i <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) i <<- inverse 
      getinverse <- function() i
      list(get = get,
           set = set,
           setinverse = setinverse,
           getinverse = getinverse)
}


## This function returns the inverse of matrix 'x'. It first checks if inverse
## has been calcutaled returns it if it exists, if not it computes the 
## inverse of 'x' using the x$setinverse function.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

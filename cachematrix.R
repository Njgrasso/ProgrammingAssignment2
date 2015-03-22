## makeCacheMatrix takes a matrix and returns a list of 4 functions which can
## be used to create a 1:1 mapping of the source matrix with its inverse in a
## cache system for easy storage and modification. 

## cacheSolve applies these functions to create this 1:1 mapping in a way that
## is non-redundant.


## Takes a Matrix and returns a list of four related functions that:
##      a) returns the user-defined matrix
##      b) returns i, a cache store for the user-defined matrix inverse
##              which is null until set to be otherwise
##      c) can be used to set a new user-defined matrix
##      d) can be used to set a new user-defined matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL  
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  v <- list(set = set, get = get, setinverse = setinverse,
            getinverse = getinverse)
}

cacheSolve <- function(v, ...) {
  ## Return a matrix that is the inverse of 'v'
  
  i <- v$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- v$get()
  i <- solve(data, ...)
  v$setinverse(i)
  i
}

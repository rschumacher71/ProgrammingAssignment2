## These two functions form the basis of a special list class which represents an invertable matrix
## The list object allows for the caching of values which will prevent unneeded recompution of the
## inverse if it has already been solved

## Function accepts as an argument an invertable matrix and creates an object of type "list"
## which represents a special "cachable" matrix.  
## List contains setter and getter functions:
## set, get, setinverse, and getinverse
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


## Function takes as an argument a specially created matrix which is created by the
## makeCacheMatrix() function and returns the inverse.  The function checks to see if a solution has
## already been determined and uses the cached value if it has, rather than recomputing the inverse.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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


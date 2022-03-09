# Notes from assignment - initial vectors

makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

## Inverse matrix cache steps

makeCacheMatrix <- function(x = matrix()) {

i <- NULL
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
 ## Get matrix

  get <- function() {
    ## Return the matrix
    m
  }
  
  ## Set inverse
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  ## Get inverse
  getInverse <- function() {
    ## Return the inverse property
    i
  }
  
  ## List of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## "cachesolve" to retrieve inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
m <- x$getInverse()
  
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data) %*% data
  x$setInverse(m)
  m
}



## set the value of the matrix
## get the value of the matrix
## calculate the inverse of the matrix
## get the value of the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- mean
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}




## Compute the inverse of the special matrix created above. 
## It checks first to see if the inverse has already been calculated. 
## If so, it gets the inverse from cache and skips the computation.
## Otherwise it calculates the inverse of the matrix and sets the inverse in the cache



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


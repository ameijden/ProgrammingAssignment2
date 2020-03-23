

## The makeCachematrix function 
## First initaliaze objects x and m
## x is initialized as a function argument, so no further initialization is required within the function
## m is set to NULL, initializing it as an object within the makeCacheMatrix() environment to be used by later code in the function
## x <<- y  # Assign the input argument to the x object in the parent environment
## m <<- NULL # Clears any value of m that had been cached by a prior execution
## get <- function() x # The symbol x is not defined within get(), R retrieves it from the parent environment of makeCacheMatrix().
## setinverse <- function(solve) m <<- solve # To assign the input argument to the value of m in the parent environment
## getinverse  <- function() m ## same as the symbol x, to find the correct symbol m to retrieve its value
## list(set = set, # gives the name 'set' to the set() function defined above
  ##   get = get,  # gives the name 'get' to the get() function defined above
 ##    setinverse = setinverse, # gives the name 'setinverse' to the setinverse() function defined above
  ##   getinverse = getinverse) # gives the name 'getinverse' to the getinverse() function defined above
##list,  assigns each of these functions as an element within a list(), and returns it to the parent environment.



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



## The cacheSolve function
## cacheSolve() starts with a single argument, x, and an ellipsis that allows the caller to pass additional arguments into the function.
## The function attempts to retrieve the inverse from the object passed in as the argument. First, it calls the getinverse() function on the input object.
## then it checks to see whether the result is NULL. Since makecacheMatrix() sets the cached mean to NULL whenever a new vector is set into the object, 
## if the value here is not equal to NULL, we have a valid, cached mean and can return it to the parent environment
## if there is no cached data then the function will calculate the inverse of the matrix

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


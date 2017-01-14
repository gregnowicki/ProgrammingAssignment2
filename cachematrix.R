## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makeCacheMatrix creates a special 'vector', which is really a list
# containing a function to:

# -- 1. set the value of the vector
# -- 2. get the value of the vector
# -- 3. set the value of the inverse
# -- 4. get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## Write a short comment describing this function
# This function calculates the inverse of the special vector created with the function above
# It first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and returns it. 
# Otherwise, it calculates it and sets the value of the inverse in the cache via the setcache function.

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if (!is.null(m)) {
      message("getting the cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}

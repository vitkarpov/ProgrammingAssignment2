## Returns a list with set of methods,
## which could be used to store and get some data from a closure.
getCachableObject <- function(x = NULL) {
  cache <- NULL
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  get <- function() {
    x
  }
  setValueToCache <- function(value) {
    cache <<- value
    value
  }
  getValueFromCache <- function() {
    cache
  }
  list(set = set, get = get,
       getValueFromCache = getValueFromCache,
       setValueToCache = setValueToCache)
}

## Returns a cachable object, which could be used
## to store the result of inverting the matrix
makeCacheMatrix <- function(x = matrix()) {
  if (ncol(x) != nrow(x)) {
    message('The matrix should be square!')
    return(NULL)
  }
  getCachableObject(x)
}

## Returns the result of inverting the matrix
cacheSolve <- function(x, ...) {
  cache <- x$getValueFromCache()
  
  if (!is.null(cache)) {
    message('The value`s been gotten from cache!')
    return(cache)
  }
  x$setValueToCache(solve(x$get(), ...))
}

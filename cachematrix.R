## matrix is passed in and a list is created with getters and setters.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  ## initialize x to value passed in and m to NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## retrieve x
  get <- function() x
  
  ## cache m to value passed in
  setcache <- function(solve) m <<- solve
  
  ## retrieve m
  getcache <- function() m
  
  ## return a list of functions
  list(set = set, get = get,
       setcache = setcache,
       getcache = getcache)
}

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  
  ##call getcache function from the list of functions passed in
  m <- x$getcache()
  
  ##if m is not null retrieve m from cache and tell the user such
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## m is null so call the get function from list of functions passed into cacheSolve()
  ## data will become the special matrix 
  data <- x$get()
  
  ##compute the inverse of the special matrix
  m <- solve(data, ...)
  
  ## the special matrix is cached 
  x$setcache(m)
  
  ##return the inverse of the special matrix
  m
}
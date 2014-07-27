# Function creates a "matrix" object, along with a get, set get-inverse and set-inverse methods.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL                 # initialise cache
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  
  # return functions to caller
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

# Function utilises the makeCacheMatrix object to calculate the inverse of the intial matrix.

cacheSolve <- function(x, ...) {
  m <- x$getinv()       # obtain result
  if(!is.null(m)) {     # if result is empty, send a message
    message("getting cached data")
    return(m)
  }
  data <- x$get()       # obtain current result
  m <- solve(data, ...) # calculate inverse matrix
  x$setinv(m)           # cache the result
  m                     # return the result
}
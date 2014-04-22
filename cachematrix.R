# Pair of functions that cache the inverse of a matrix

# Create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  set_mat_inv <- function(mat_inv) m <<- mat_inv
  get_mat_inv <- function() m
  list(set = set, get = get, set_mat_inv = set_mat_inv, 
       get_mat_inv = get_mat_inv)
}

# Compute the inverse of the special "matrix" returned by makeCacheMatrix. If 
# the inverse has already been calculated (and the matrix has not changed), then 
# cacheSolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
  m <- x$get_mat_inv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$set_mat_inv(m)
  m
}

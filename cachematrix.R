## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ##Initialize the inverse matrix
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ##Method to get the matrix
  get <- function() 
    ##Return the matrix
    x
  ##Make the inverse of the matrix
  setinverse <- function(inverse) 
  m <<- inverse
  ##Make the inverse of the matrix
  getinverse <- function() 
    ##Return the inverse
    m
  ##List of the methods
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## Back to a matrix "m"
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  ##Set inverse to object
  x$setinverse(m)
  ##Back to matrix
  m
}

## This function calculate inverse of a square matrix. If inverse is already been calculated , it will return cached value. 
## Below function does following things -  set the value of the matrix, get the value of the matrix, set the value of inverse of the matrix,  get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
m <- NULL
## setting the value of matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## Return matrix to calculate inverse
  get <- function() x
  ## Set the value of inverse to cache
  setInverse <- function(inverse) m <<-inverse
  ## Return the cache value of inverse Or return the null
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
## Write a short comment describing this function
## Below function returns inverse of matrix. It checks first , if inverse already been calculated and return cached value else it will calculate inverse. 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		## It gets cached value of matrix
		 m <- x$getInverse()
		## It checks , if value of m is null. If it is not null. It returns that value.
     if ( ! is.null(m)) {
    print("getting cached Inverse")
    return(m)
  }
  ## If inverse has not been calculated ( i.e. m is null). It calculates inverse below.
  m <- solve(x$get())
  ## It cached value of matrix inverse
  x$setInverse(m)
  ## Return the value of m ( inverse of matrix) to calling environment.
  m
  }

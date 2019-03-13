## This function calculate inverse of a square matrix. If inverse is already been calculated , it will return cached value. 
## Below function does following things -  set the value of the matrix, get the value of the matrix, set the value of inverse of the matrix,  get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
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


## Sample run:
## > x = rbind(c(1, -1/4), c(-1/4, 1))
## > m = makeCacheMatrix(x)
## > m$get()
##       [,1]  [,2]
## [1,]  1.00 -0.25
## [2,] -0.25  1.00

## No cache in the first run
## > cacheSolve(m)
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667

## Retrieving from the cache in the second run
## > cacheSolve(m)
## getting cached data.
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
## > 

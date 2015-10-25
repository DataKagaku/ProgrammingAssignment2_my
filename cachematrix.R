## For Programming Assignment 2 of Coursera course: R programming.
## These are a pair of functions that cache the inverse of a matrix.


## The first function, makeCacheMatrix, creates a special "matrix" object that can cache 
## its inverse, which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the matrix inverse
## 4. get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {    ## anonymous function for setting the value of the matrix
    x <<- y
    m <<- NULL
  }
  get <- function() x     ## get the value of the matrix
  set_inverse <- function(mat_inverse) m <<- mat_inverse    ## set the value of the matrix inverse
  get_inverse <- function() m   ## get the value of the matrix inverse
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## The second function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$get_inverse()
  if(!is.null(m)) {   ## check if the matrix has been changed
    message("getting cached data of matrix inverse")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)   ## calculate the inverse of the matrix
  x$cal_inverse(m)
  m
}

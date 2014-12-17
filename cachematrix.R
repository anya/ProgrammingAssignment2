##  Programming assignment 2: 
##  A pair of functions that enable caching the inverse of a matrix

## makeCacheMatrix() is a function that creates a special matrix object 
## containing, along with the data of the matrix originally passed in, 
## the four accessor functions, which provide access to that data, 
## as well as to the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL

  ## get() returns stored matrix data
  get <- function() x

  ## set() modifies matrix data by setting it to passed in value
  ## note that once the matrix is modified, its cached inverse becomes null
  set <- function(y) {
    x <<- y 
    i <<- NULL
  }

  ## getinverse() returns the cached inverse matrix (NULL if never computed)
  getinverse <- function() i
  
  ## setinverse() assigns the internally stored (cached) inverse a new value
  setinverse <- function(newinverse) i <<- newinverse
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve() operates on a matrix object created using makeCahceMatrix() and 
## returns its inverse matrix.
## It checks if the internally stored (cached) inverse is not NULL; if so,
## it returns that matrix instead of recalculating it; otherwise it computes the
## inverse, saves it in a cache for future use, and returns it.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()   ## using accessor function to obtain cached inverse
  if (!is.null(i)) {
    message("getting cached inverse matrix")
    return(i)
  }
  message("computing inverse matrix anew")
  data <- x$get()   ## obtain matrix data
  i <- solve(data, ...) ## compute new inverse
  x$setinverse(i)   ## save computed matrix
  i
}

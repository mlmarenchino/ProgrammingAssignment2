## Coursera R Course: Caching the Inverse of a Matrix
## As the matrix inversion operation is costly, we compute
## it once and cached the result.


## Function to create a vector object containing a matrix
## and its inverse (in case it was computed)
makeMatrix <- function(x = matrix(numeric())) {
  a <- NULL
  set <- function(y) {
    x <<- y
    a <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) a <<- inverse
  getinverse <- function() a
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function to compute the inverse of a matrix.
## The function computes the inverse or obtains it 
## from the cached value if it was computed before.
cacheinverse <- function(x, ...) {
  a <- x$getinverse()
  if(!is.null(a)) {
    message("getting cached inverse of the matrix")
    return(a)
  }
  mat <- x$get()
  a <- solve(mat)
  x$setinverse(a)
  a
}


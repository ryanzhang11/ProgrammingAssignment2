## Since it may take too long to do matrix inversion,
## it is much more resonable to cach the inverse of a matrix,
## rather than compute the it every single time.
## The following solution can achieve this goal.

## The first function creates an object to cache the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
  inve <- NULL
  set <- function(y){
    x <<- y
    inve <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse)inve <<-inverse
  getInverse <- function()inve
  list(set=set,
       get=get,
       setInverse=setInverse,
       getInverse=getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    inve <- x$getinverse()
    if(!is.null(inve)) {
      message("getting cached data.")
      return(inve)
    }
    data <- x$get()
    inve <- solve(data)
    x$setinverse(inve)
    inve
}

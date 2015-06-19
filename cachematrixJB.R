## As professor Peng stated in the problem statement
## Matrix inversion is usually a costly computation
## and there may be some benefit to caching the inverse of a
## matrix rather than computing it repeatedly.

## The first function, makeCacheMatrix, is a list
## containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = numeric()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() {
      x
    }
    
    setinverse <- function(inverse) {
      inv <<- inverse
    }
    getinverse = function() {
      inv
    }
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## This next function returns the inverse of the matrix. 
## It checks to see if the inverse has already been
## computed. If this is the case, the function gets
## the results and skips the computation. If this
## is not the case, the function sets the value in
## the cache via the above setinverse function.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data; please wait.")
    return(inv)
  }
  data <- x$get()
  inv <-solve(data, ...)
  x$setinverse(inv)
  inv

}

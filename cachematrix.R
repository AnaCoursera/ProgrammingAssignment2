#Matrix inversion is usually a costly computation and there may be some benefit to
#caching the inverse of a matrix rather than compute it repeatedly (there are also
#alternatives to matrix inversion that we will not discuss here).
#Our assignment is to write a pair of functions that cache the inverse of a matrix.

#The first function, makeCacheMatrix creates a special "matrix", object that can
#cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  #1 - set the value of the matrix
  inver <- NULL
  set <- function(y){
    x<<-y
    inver<<- NULL
  }
  #2 - get the value of the matrix
  get <-function()x
  #3 - set the value of the inverse
  setinver <-function(inverse) inver<<- inverse
  #4 - get the value of the inverse
  getinver <- function()inver
  list(set=set, get=get, setinver=setinver, getinver=getinver)
}

#The following function calculates the inverse of the special "matrix" created with
#the above function.
#However, it first checks to see if the inverse has already been calculated.
#If so, it gets the inverse from the cache and skips the computation.
#Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinver function.

#For this assignment, assume that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
  inver <- x$getinver()
  if (!is.null(inver)){
    message("getting cached data")
    return(inver)
  }
  data <- x$get()
  inver<-solve(data,...)
  x$setinver(inver)
  inver
}
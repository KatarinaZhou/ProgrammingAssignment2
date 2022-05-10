## The first function sets up a vector that is generally
##  a list that include setting/getting the value of the matrix 
##  and setting/getting the inverse.
  


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
          x <<- y
          i <<- NULL
  }

  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
## The second function computes the inverse of matrix
## created by the first function
## if an inverse is already computed, then this function gets
## the inverse from the cache without the calculation. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
      message("getting cached data")
      return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

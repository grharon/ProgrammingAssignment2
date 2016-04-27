# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

## makeCacheMatrix: return a list of functions to:
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse
## 4. Get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  ## Initialize the inverse property
  m<-NULL
  
  ## Method to set the matrix
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  
  ## Method the get the matrix
  get <- function() {
    ## Return the matrix
    x
  }
  
  ## Method to set the inverse of the matrix
  setInverse <- function(inverse) {
    m <<- inverse
  }
  
  ## Method to get the inverse of the matrix
  getInverse <- function() {
    ## Return the inverse property
    m
  }
  
  ## Return a list of the methods
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# The cacheSolve function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  
  ## Return the inverse if its already set
  if( !is.null(inv) ) {
    message("Successful in getting cached data")
    return(inv)
  }
  
  ## Get the matrix from our object
  data <- x$get()
  
  ## Calculate the inverse using matrix multiplication
  inv <- solve(data)
  
  ## Set the inverse to the object
  x$setInverse(inv)
  
  ## Return the matrix
  inv
}

# Example usage:
x <- matrix(c(1, 3, 1, 1, 1, 2, 2, 3, 4),nrow=3, ncol=3, byrow=TRUE) 
##      [,1] [,2] [,3]
##[1,]    1    3    1
##[2,]    1    1    2
##[3,]    2    3    4
x

m = makeCacheMatrix(x)
##      [,1] [,2] [,3]
##[1,]    1    3    1
##[2,]    1    1    2
##[3,]    2    3    4
m$get()

# No cache in the first run
##     [,1] [,2] [,3]
##[1,]    2    9   -5
##[2,]    0   -2    1
##[3,]   -1   -3    2
cacheSolve(m)
## Retrieving from the cache in the second run
## Successful in getting cached data
##     [,1] [,2] [,3]
##[1,]    2    9   -5
##[2,]    0   -2    1
##[3,]   -1   -3    2
cacheSolve(m)
 
## check inverse value result
##      [,1] [,2] [,3]
##[1,]    1    0    0
##[2,]    0    1    0
##[3,]    0    0    1
inv <- cacheSolve(m)
x %*% inv

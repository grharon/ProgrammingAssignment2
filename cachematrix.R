## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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
    i <<- inverse
  }
  
  ## Method to get the inverse of the matrix
  getInverse <- function() {
    ## Return the inverse property
    m
  }
  
  ## Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  
  ## Return the inverse if its already set
  if( !is.null(inv) ) {
    message("getting cached data")
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
inv1 <- cacheSolve(m)
inv1
## Retrieving from the cache in the second run
##     [,1] [,2] [,3]
##[1,]    2    9   -5
##[2,]    0   -2    1
##[3,]   -1   -3    2
inv2 <- cacheSolve(m)
inv2
 
## check inverse value
x %*% inv1
x %*% inv2

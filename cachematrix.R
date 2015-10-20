## Matrix inversion is an invaluable tool when solving matrix equations. It is however a
## tedious as well as computationaly expensive task. The following pair of functions take a matrix and
## output its inverse matrix in a computationaly effecient way. 

## Input a matrix named x (must be a square matrix), store the results of the makeCacheMatrix in  
## example = x = matrix(c(2,3,4,5), nrow=2, ncol=2) 

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  }


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache

## The function below checks the vector created above and determines if the inverse of matrix has been calculated
## If it hasn't it calculates the inverse of the matrix... if it has it returns the inverted matrix without
## 

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

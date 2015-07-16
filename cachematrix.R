## This function makeCacheMatrix will create a special "matrix" object,
## which is actually a list that contains four functions:
## 1. set: Sets the values of the matrix
## 2. get: Gets the values of the matrix
## 3. setinverse: Sets the value of the matrix inverse
## 4. getinverse: Gets the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse)  inv<<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The following function calculates the  inverse of the matrix
## created with the above function. 
## It first checks to see if the inverse has already been calculated (via getinverse function)
## If so, it gets the inverse from the cache and skips the computation. 
## If not, it calculates the inverse of the matrix and 
## sets the value obtained of the inverse in the cache
## via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data_matrix <- x$get()
  inv <- solve(data_matrix, ...)
  x$setinverse(inv)
  inv
}

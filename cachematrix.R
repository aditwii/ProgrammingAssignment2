## These two functions cache the inverse of a matrix.
## makeCacheMatrix stores a matrix and can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) inv <<- inverse
  
  getinverse <- function() inv
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve computes or retrieves the inverse of that matrix.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  
  if (!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

##Test
mat <- matrix(c(2,1,1,2),2,2)
specialMat <- makeCacheMatrix(mat)
cacheSolve(specialMat)
           [,1]       [,2]
[1,]  0.6666667 -0.3333333
[2,] -0.3333333  0.6666667
cacheSolve(specialMat)
getting cached inverse
           [,1]       [,2]
[1,]  0.6666667 -0.3333333
[2,] -0.3333333  0.6666667
>

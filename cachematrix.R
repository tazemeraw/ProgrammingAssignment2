## This script creates 2 functions (makeCacheMatrix and cachesolve)

## makeCacheMatix funtion creates an R object to store a Matrix (x) and it's Inverse (m)


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve #calculate the inverse
  getInverse <- function() m
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve looks to see if the Inverse has already been calulated and if so retrives it from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}


# below are some commands to use to test the script. Note that once myMatrix is loaded the cacheSolve
# function needs to calculate the inverse. The second time it will get the inverse from cache.

# myMatrix <-makeCacheMatrix(matrix(c(1,0,0,0,1,0,0,0,2),ncol=3,nrow=3))
# cacheSolve(myMatrix)
# myMatrix$get()
# myMatrix$getInverse()
# cacheSolve(myMatrix)


# myMatrix <-makeCacheMatrix(matrix(c(1,2,3,3,2,1,2,3,1),ncol=3,nrow=3))
# cacheSolve(myMatrix)
# myMatrix$get()
# myMatrix$getInverse()
# cacheSolve(myMatrix)

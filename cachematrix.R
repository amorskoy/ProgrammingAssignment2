## Caching inversion of invertible (as assumed) matrix
## Test case:
## a <- matrix(c(3,1,2,1),nrow=2,ncol=2)
## ac = makeCacheMatrix(a)
## sac = cacheSolve(ac)
## sac = cacheSolve(ac)
## You should see: 
## 1. On both calls of cacheSolve - inverted result = matrix(c(1,-1,-2,3), 2, 2) 
## 2. On the second call of cacheSolve - message "getting cached data"
## 3. Result of round(ac$get() %*% sac) equal to matrix(c(1,0,0,1),2,2)

## Prepare version of matrix supporing cached inversion result

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinvert <- function(invert) i <<- invert
  getinvert <- function() i
  list(set = set, get = get,
       setinvert = setinvert,
       getinvert = getinvert)
}


## Return cached inverted value for invertable matrix x.
## If value is not cached - then it calculates inversion first 
## and returns the result.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  i <- x$getinvert()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  data <- x$get()
  i <- solve(data, ...)
  x$setinvert(i)
  
  return(i)
}

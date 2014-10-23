## This program calculates and caches the inverse value of a matrix

## This function creates a special matrix. It sets the value of the matrix, gets the value of the matrix, sets the inverse of 
## the matrix, and gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL      #assign local variable m
  set <- function(y) {
    x <<- y     
    m <<- NULL   #assign containing environment m
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve #assign containing environment m
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache. Otherwise,
## it calculates the inverse and sets the value in the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse() #assign local variable m to whatever value is in the env
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...) #assign local variable m 
  x$setinverse(m)       #this function will assign the set inverse value to m in the containing env
  m
}

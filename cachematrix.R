## Calculate inverse and save in cache.

## Create functions to calculate inverse and save in cache.

makeCacheMatrix <- function(x = matrix()) 
{
  m <- NULL
  set <- function(y) 
  {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function calculates inverse. If inverse exists, it get it from cache.

cacheSolve <- function(x, ...) 
{
  m <- x$getinverse()
  if(!is.null(m)) 
  {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m ## Return a matrix that is the inverse of 'x'
}
#sets and returns the cache
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x #returns x if inverse not present in cache
  setinverse <- function(inverse) i <<- inverse #sets inverse in cache
  getinverse <- function() i #returns i 
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
#checks the cache and solves for the inverse of the matrix
cacheSolve <- function(x, ...) {
  i <- x$getinverse() #checks if the data is there in cache
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get() #gets x
  i <- solve(data, ...) #inverse of x
  x$setinverse(i) #sets inverse in cache
  i
}
## Put comments here that give an overall description of what your
## functions do

## create a set of functions to cache set/retrieve


makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve(m)
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## return cached item if we have one...or create if needed

cacheSolve <- function(x, ...) {
        
  ## Return a matrix that is the inverse of 'x'
   
   m <- x$getinv()
   if(!is.null(m)) {
     message("getting cached data")
     return(m)
   }
   data <- x$get()
   m <- solve(data, ...)
   x$setinv(m)
   m
  
}

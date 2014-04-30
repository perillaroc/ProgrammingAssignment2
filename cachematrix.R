## calculate the inverse of a matrix and cache its result in it 
## for future use

## create a object which has a matrix and caches its inverse 

makeCacheMatrix <- function(x = matrix()) {
  ## init cached inverse matrix
  inve <- NULL
  
  ## set matrix in this object
  set <- function(y) {
    x <<- y
    inve <<- NULL
  }
  
  ## get matrix
  get <- function() x
  
  ## set inverse matrix
  setinve <- function(i) inve <<- i
  
  ## get inverse matrix
  getinve <- function() inve
  
  ## return this list object as a cached matrix.
  list(set = set, get = get,
       setinve = setinve,
       getinve = getinve)
}


## Calculate the inverse of the matrix in x, and caches it in x.
## Just return the cache matrix When call this function again.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  # get the cached matrix
  inve <- x$getinve()
  
  ## if there is a cached matrix, return it.
  if(!is.null(inve)) {
    message("getting cached data")
    return(inve)
  }
  
  ## if there is not a cached matrix, calculate the inverse.
  data <- x$get()
  inve <- solve(data, ...)
  x$setinve(inve)
  inve
}
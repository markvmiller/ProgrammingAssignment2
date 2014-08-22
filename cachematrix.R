#These combined functions serve to cache the inverse of a matrix 
#and either call that stored inverse or calculate it from scratch.

## Stores cached version of matrix. It either will set the matrix 
#or get it 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmat <- function(solve) m <<- solve
  getmat <- function() m
  list(set = set, get = get,
       setmat = setmat,
       getmat = getmat)  
}


## Produces the inverse of a matrix: it first sees if the value has
#already been cached. If so, it uses that. If not, the function calculates 
#the inverse on its own.

cacheSolve <- function(x, ...) {
  m <- x$getmat()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmat(m)
  m 
}

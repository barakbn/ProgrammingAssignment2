## Matrix inversion is usually a costly computation and their may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly. 
## The following two functions allow us to create such an object that holds 
## both a matrix and its inverse, so that when the matrix' inverse is retrieved, the cached 
## copy will be returned (if such copy exists) or will be calculated if necessary.

## makeCacheMatrix creates a list that holds four functions: 
## set -- for setting a new matrix  (setting new matrix will also delete any stored inverse, if that existed)
## get -- for getting the matrix
## setInverse -- for setting the matrix inverse
## getInverse -- for getting the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve receives the 'CacheMatrix' (created by makeCacheMatrix), 
## checks if a stored inverse exists (by calling the getinverse function)
## if such a stored inverse does exist, the function returns it, 
## otherwise it computes the inverse for the given matrix
## sets the calculated inverse to be cached (using setinverse), and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

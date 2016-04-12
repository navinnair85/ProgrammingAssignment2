## The following functions cache the inverse of a Matrix which help 
## avoid unnecessary costly computation 

## makeCacheMatrix creates a list containing a function to 
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)  
}


##cacheSolve retrieves the inverse of the "matrix" from
#the cache if it has already been calculated. If not,  
#cacheSolve computes the inverse of the matrix using the 
#solve function and stores the result in the cache using
#the setInverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
  
}

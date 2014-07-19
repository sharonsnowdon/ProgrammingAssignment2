# The following functions cache the inverse of a matrix, 
# so that it does not have to be repeatedly calculated

# Function that creates a list of four functions that set the matrix, 
# get the matrix,set the inverse and get the inverse 

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
      set <- function(y) {
                x <<- y
                inv <<- NULL
      }
      get <- function() x
	print(x)
      setinverse <- function(solve) inv <<- solve
      getinverse <- function() inv
    	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# Function that checks the cache, and returns the inverse if it has been 
# previously calculated and stored.
# Otherwise the inverse is calculated, stored and returned.

cacheSolve <- function(x, ...) {
     	# Returns a matrix that is the inverse of 'x'
	inv <- x$getinverse()
      if(!is.null(inv)) {
         	message("getting cached data")
          	return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinverse(inv)
      inv
}


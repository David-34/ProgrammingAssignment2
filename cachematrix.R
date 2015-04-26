## This is my solution to Programming Assignment 2
## The makeCacheMatrix function creates a "special matrix" that can cache the inverse of the matrix provided.
## The cacheSolve function returns a inverse matrix from the cache if available. If the inverse is not yet in
## the cache, the function calculates the inverse and adds it to the cache.


## Given a matrix x, return "special matrix" that can contain a cached inverse matrix
makeCacheMatrix <- function(x = matrix()) {
          
        inverse <- NULL
        
        set <- function(y) {
        x <<- y
                inverse <<- NULL
        }
        get <- function() x
        
        setinverse <- function(inv) inverse <<- inv
        getinverse <- function() inverse

        list(set = set, get = get,
            setinverse = setinverse,
            getinverse = getinverse)
}


## Given a "special matrix" that can cache an inverse, return the inverse (from cache if available)
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
        inv <- x$getinverse()
        
        if(is.null(inv)) {
          ## Inverse not yet in cache, so add it now
          data <- x$get()
          inv <- solve(data, ...)
          x$setinverse(inv)
          message("Calculated inverse matrix and added it to cache")
        }

        ## inv is now guaranteed to be available so return it
        inv
}

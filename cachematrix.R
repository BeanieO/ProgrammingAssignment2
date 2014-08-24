## The pair of functions makeCacheMatrix and cacheSolve together compute and cache the 
## inverse of a matrix and enable it to be retrieved from the cache again so that it need 
## not be recalculated, thereby reducing computations. 


## The makeCacheMatrix function takes the input matrix 'x' and creates a special "matrix" 
## object that can cache and retrieve the inverse of 'x'. This object can then be used as
## the input to the second function, cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }

        get <- function() { x }
        
        setinverse <- function(inverse) { m <<- inverse }
        
        getinverse <- function() { m }
        
        list(set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse)
        
}


## The cacheSolve function takes the special "matrix" object returned by the function 
## makeCacheMatrix and returns the inverse of the original input matrix 'x'. 
## If the inverse of 'x' has not yet been calculated or if 'x' has changed since its inverse
## was last calculated, the inverse is calculated using the R function solve.
## If the inverse of 'x' has already been calculated, it is retrieved from the cache.

cacheSolve <- function(z, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        m <- z$getinverse()
        
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        data <- z$get()
        
        m <- solve(data, ...)
        
        z$setinverse(m)
        
        m  
  
}

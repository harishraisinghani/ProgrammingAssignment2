## This function creates a list of functions: set, get, setInverse, getInverse for a matrix input

makeCacheMatrix <- function(x = matrix()) {

        m <- matrix()
        set <- function(y) {
                x <<- y
                m <<- matrix()
        }
        
        get <- function() x
        
        setInverse <- function(inverse) {
                m <<- inverse
        }
        
        getInverse <- function() m
        
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}
        


## This function takes in a matrix argument and returns its inverse. The inverse is either returned from 
## cache if it has already been calculated or is calculated and stored in cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getInverse()
        
        if(!is.na(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        
        x$setInverse(m)
        
        m
        
}

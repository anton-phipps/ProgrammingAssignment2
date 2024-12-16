## These are functions to save data to a cache for speed of access
## and a helper function to invert a matrix if it has not already
## been complete through other means

## This is a function to store and retreive information from a matrix. Data
## can be both assigned and retreived when needed.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<-inverse
        getinverse <- function() m
        list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## This function checks to see if the matrix has already been inverted, if so
## then it will return the cached value if it has not been inverted it calls
## the solve function to invert the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        return(m)
}

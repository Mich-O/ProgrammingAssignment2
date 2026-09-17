## These functions create a special matrix object that stores
## a matrix and its cached inverse. The inverse is calculated
## only when it has not already been cached.

## Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL

        set <- function(y) {
                x <<- y
                m <<- NULL
        }

        get <- function() x

        setinverse <- function(inverse) {
                m <<- inverse
        }

        getinverse <- function() m

        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Computes the inverse of the special matrix.
## If the inverse has already been calculated, it
## retrieves the cached inverse instead.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()

        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }

        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)

        m
}

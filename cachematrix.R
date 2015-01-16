## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- matrix(nrow=0, ncol=0)
        set <- function(y) {
                x <<- y
                inv <<- matrix(nrow=0, ncol=0)
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(ncol(inv) > 0) {
                message("getting cached data")
                return(inv)
        }
        orig <- x$get()
        inv <- solve(orig)
        x$setinverse(inv)
        inv
}

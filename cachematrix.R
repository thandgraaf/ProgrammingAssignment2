## These two functions make it possible to calculate and cache the inverse of a matrix
## Usage (assumption: m is a square, invertible matrix)
## cm <- makeCacheMatrix(m)
## cacheSolve(cm) 
## repeated calls of cacheSolve(cm) return the cached inverse without recomputation

## makeCacheMatrix takes a matrix as argument
## It stores (caches) the matrix and its inverse in a separate environment.
## It returns a list of four functions that can be used as follows:
## After calling cm <- makeCacheMatric(M),
## cm$set(x) sets the matrix in cm and will reset the inverse to NULL
## cm$get() returns the matrix cached in cm
## cm$setinverse(x) sets the inverse in cm
## cm$getinverse(x) returns the inverse cached in cm

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve uses the value obtained from makeCacheMatrix as argument
## if a cached value of the matrix exists it is returned
## otherwise the inverse is calculated (and cached)

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse) > 0) {
                message("getting cached data")
                return(inverse)
        }
        original <- x$get()
        inverse <- solve(original)
        x$setinverse(inverse)
        inverse
}


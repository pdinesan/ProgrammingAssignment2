## Matrix inversion and caching

## makeCacheMatrix takes an argument of type matrix and
## returns an object with associated functions to get and set the date of the Matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
set <- function(m){ 
    x <<- m
    inv <<- NULL
}
get <- function() x
setInverse <- function(parInv) inv <<- parInv
getInverse <- function() inv
list(set = set, get = get,
     setInverse = setInverse,
     getInverse = getInverse)
}

## cacheSolve checks if the incoming object already has a cached data, if yes, it will return the cached inverse of the matrix
## else it will inverse the matrix and store in the cache

cacheSolve <- function(x, ...) {
    Invmat <- x$getInverse()
    if(!is.null(Invmat))
    {
        message("getting cached data")
        return (Invmat)
    }
    data <- x$get()
    Invmat <- solve(data, ...)
    x$setInverse(Invmat)
    Invmat
}

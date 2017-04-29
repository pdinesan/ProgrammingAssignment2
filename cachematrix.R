## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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

## Write a short comment describing this function

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

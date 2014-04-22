## Two functions to provide a cacheable interface to matrices

## Creates a special matrix-like object able to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(calcInv) inv <<- calcInv
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Computes the inverse of a special object created by makeCacheMatrix
## checking the cache first (if the matrix has not been changed)

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if (!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}

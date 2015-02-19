## functions get, set, getinv and set inv
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(x) x <<- inv
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## find inverse of matrix or 
## if previously found, return inverse from cache
cacheSolve <- function(x, ...) {
        matInv <- x$getinv()
        if(!is.null(matInv)) {
                message("getting cached data")
                return(matInv)
        }
        data <- x$get()
        matInv <- solve(data, ...)
        x$setinv(x)
        matInv
}

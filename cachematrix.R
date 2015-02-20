## constructor functions for get, set, setinv and getinv
makeCacheMatrix <- function(mat = matrix()) {
        inv <- NULL
        set <- function(y) {
                mat <<- y
                inv <<- NULL
        }
        get <- function() mat
        setinv <- function(inverse) inv <<- inverse
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
        matInv <- solve(data)
        x$setinv(matInv)
        matInv
}

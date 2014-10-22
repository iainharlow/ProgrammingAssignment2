## These two functions set up a framework in which matrix inverses can be 
## calculated, cached, and retrieved. The functions to set and retrieve the
## cached inverse are constructed with makeCacheMatrix; cacheSolve uses these 
## functions to check whether an inverse has been calculated and stored (and to
## retrieve it if it has, or to calculate and set it if it hasn't).

## makeCacheMatrix creates the matrix along with four functions to $set its 
## value; $get / retrieve the matrix; $setinverse (cache the inverse when later 
## calculated) and $getinverse (retrieve the cached inverse if present):

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
            x <<- y
            i <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) i <<- inv
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve checks using $getinverse whether the inverse has been calculated
## and cached previously. If so, it returns this value. If not, it calculates
## the inverse and caches it for later retrieval (using $setinverse):

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}

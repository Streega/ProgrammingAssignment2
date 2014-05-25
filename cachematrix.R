## Function makeCacheMatrix creates a special "matrix" object that can cache its
## inverse.

makeCacheMatrix <- function(x = matrix()) {
    c <- NULL
    set <- function(b) {
        x <<- b
        c <<- NULL
    }
    get <- function() x
    setinv <- function(solve) c <<- solve
    getinv <- function() c
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## Function cacheSolve computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the  cachesolve  should retrieve the inverse
## from the cache.

cacheSolve <- function(x, ...) {
    ptm <- proc.time()       ## Return a matrix that is the inverse of 'x'
    c <- x$getinv()          ## Get Matrix "c" from cache
    if(!is.null(c)) {        ## Check if not null, return value read from cache
        cat("Process Time", proc.time() - ptm)
        return(c)
    }
    mydata <- x$get()        ## If "c" is NULL, get origal Matrix
    c <- solve(mydata, ...)  ## Calculate inverse using solve
    x$setinv(c)
    cat("Process Time", proc.time() - ptm)
    c                        ## Returm inverse
}
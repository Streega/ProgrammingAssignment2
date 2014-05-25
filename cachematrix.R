## Function makeCacheMatrix creates a special "matrix" object that can cache its
## inverse.

makeCacheMatrix <- function(x = matrix()) {
    c <- NULL               ## Initialize value of inverse matrix 'c' 
    set <- function(b) {    ## Set value of matrix from user input 'b'
        x <<- b             ## Set to global 'x' value of matrix input 'b'
        c <<- NULL          ## Clear inverse matrix 'c' value if it exists
    }
    get <- function() x     ## Get value of matrix if it has been set
    setinv <- function(solve) c <<- solve ## Set value of the inverse of matrix
    getinv <- function() c  ## Get value of the inverse matrix 'c'
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Function cacheSolve computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the  cachesolve  should retrieve the inverse
## from the cache.

cacheSolve <- function(x, ...) {
    ptm <- proc.time()       
    c <- x$getinv()          ## Get inverse matrix 'c' from cache
    if(!is.null(c)) {        ## Check if not NULL, return value read from cache
        message("Getting cached data")
        cat("Process Time", proc.time() - ptm)
        return(c)
    }
    mydata <- x$get()        ## If inverse matrix 'c' is NULL, get origal matrix
    c <- solve(mydata, ...)  ## Calculate inverse using solve
    x$setinv(c)
    cat("Process Time", proc.time() - ptm)
    c                        ## Returm inverse matrix 'c'
}

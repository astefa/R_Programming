## This code aims to save computation time by caching the inverse of
## a given matrix so that if it is required again, it does not need
## to be computed again, instead being retrieved directly from the
## cache.

## This function creates a special matrix object that can cache its
## inverse.

makeCacheMatrix <- function(x = matrix()) {
    v <- NULL
    set <- function(y) {
        x <<- y
        v <<- NULL
    }
  
    get <- function() x
    setinv <- function(solve) v <<- solve
    getinv <- function() v
    list(set = set, get = get, setinv = setinv, 
         getinv = getinv)  
}


## This function will compute the inverse of the special matrix
## object created above. If the inverse has already been calculated,
## then this function will retrieve it directly from the cache.

cacheSolve <- function(x, ...) {
    ## This code checks if the inverse has already been calculated
    ## and returns it if it has. 
    v <- x$getinv()
    if(!is.null(v)){
        print("Getting cached inverse...")
        return(v)
    }
    
    ## If no inverse has already been calculated, this code returns
    ## the inverse of the supplied matrix object. 
    data <- x$get()
    m <- solve(data,...)
    x$setinv(m)
    m
}
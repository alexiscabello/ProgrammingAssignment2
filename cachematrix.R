## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    z <- x
    set <- function(y) {
        if(!(dim(z) && dim(y) && all(z == y)))
        {
            z <<- y
            m <<- NULL
        }
    }
    get <- function() z
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set,get = get,
         setinv = setinv,
         getinv = getinv)    
    
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        #message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data,...)
    x$setinv(m)
    m    
}

## This function creates a special "matrix" object that can cache its inverse

## It takes a matrix as input parameter

makeCacheMatrix <- function(x = matrix()) {
    
    s <- NULL
    
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function(solve) s <<- solve 
    
    getinverse <- function() s
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), then the 
## cacheSolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
    
    s <- x$getinverse()
    
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    
    data <- x$get()
    s <- solve(data, ...)
    
    x$setinverse(s)
    
    ## Return a matrix that is the inverse of 'x'
    
    s       
}

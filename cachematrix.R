## To avoid repeating potentially computing intensive operations, we can create
## result cache for later retrieval by other functions. 
##  
## Two functions are written below specially to cache the inverse of a matrix. 
##
## The first function is "makeCacheMatrix". This function creates a special 
## "matrix" object that can cache its inverse. It contains 4 sub-functions to 
##     set, get, getinverse and setinverse of the matrix.
## The user should firstly call v<-makeCacheMatrix(m) where m is a square matrix
## and v is the object to contain the returned results in a list format.
##
## The second function is "cacheSolve" which computes the inverse of the special 
## "matrix" returned by "makeCacheMatrix" above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.
##

## makeCacheMatrix contains 4 sub-functions
##      set: Store the matrix and set the inverse value to NULL
##      get: Return the matrix
##      setinverse: Returns the inverse of the matrix
##      getinverse: Returns the inverse
## makeCacheMatrix returns a list of the above 4 functions which will then implicitly
## store the value of the matrix as well as the latest calculated inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(mean) m <<- mean
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve accepts a list x containing matrix functions produce by makeCacheMatrix.
## If the cached result is already present then return the cached inverse result. 
## Otherwise calculate the inverse and store in the cache.

cacheSolve <- function(x, ...) {
    ## First, get the current inverse cached value
    m <- x$getinverse()

    ## If the cache is not empty, then return the cache value.
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ## Otherwise, get the cache matrix ...
    data <- x$get()
    
    ## calculate the inverse ...
    m <- solve(data, ...)
    
    ## set the inverse result into the cache and return the inverse result.
    x$setinverse(m)
    m
}

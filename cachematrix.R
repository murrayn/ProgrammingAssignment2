##
## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than computing it repeatedly.
## 
## Usage:
##
##    mat <- matrix( ....
##    cachedMat<- makeCacheMatrix(mat)      # Create the cached matrix object
##    inversedMat <- cacheSolve(cachedMat)  # Compute its inverse and cache the result
##    inversedMat2 <- cacheSolve(cachedMat) # This result will come from the cache
##    cachedMat$set(inversedMat2)           # Set the original matrix to the inverse
##    cacheSolve(cachedMat)                 # This will return the original matrix


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

    theMatrix <- NULL
    
    set <- function(y = matrix()) {
        x <<- y
        theMatrix <<- NULL
    }
    
    get <- function() x
    
    setInverse <- function(y) theMatrix <<- y
    
    getInverse <- function() theMatrix
    
    list(setInverse = setInverse, getInverse = getInverse, get = get, set = set)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cachesolve will retrieve the inverse
## from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    Inverse <- x$getInverse()
    if(!is.null(Inverse)) {
        message("getting cached data")
        return(Inverse)
    }
    Inverse <- solve(x$get())
    theMatrixxxxx <- x$setInverse(Inverse)
    theMatrixxxxx
}

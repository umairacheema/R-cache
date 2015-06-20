## These functions demonstrate how to use caching in R to speed computation
## In this particular case we store inverse of a matrix in cache and return
## cached version if it exists in the cache or compute inverse and store in 
## cache
## Example : mat<-cbind(c(1,2),c(2,3))
##           x<-makeCacheMatrix(mat)
##           cacheSolve(x)
##           The first call to cacheSolve above will save inverse in cache
##           cacheSolve(x)
##           This call will just retrieve the matrix inverse from cache

## makeCacheMatrix
## This function caches the Inverse of a matrix in the xInverse variable
## and provides methods to set or get this variable

makeCacheMatrix <- function(x = matrix()) {
        xInverse <- NULL
        set <- function(y) {
                x <<- y
                xInverse <<- NULL
        }
        get <- function()
                x
        setInverse <-
                function(matrixInverse)
                        xInverse <<- matrixInverse
        getInverse <- function()
                xInverse
        list(
                set = set,get = get,setInverse = setInverse,getInverse = getInverse
        )
}


## cacheSolve
## This function takes a matrix, checks if the inverse of the matrix
## exists in the cache, if it exists, the inverse is returned from the
## cache otherwise the function computes the inverse and caches it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matrixInverse <- x$getInverse()
        if (!is.null(matrixInverse)) {
                message("getting cached matrix inverse")
                return(matrixInverse)
        }
        data <- x$get()
        matrixInverse <- solve(data,...)
        x$setInverse(matrixInverse)
        matrixInverse
        
}

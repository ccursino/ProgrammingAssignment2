## This two functions can be used to calculate the inverse of a matrix, and cache the result.
## Sample usage:
##   myMatrix <- makeCacheMatrix( matrix(c(1,0,5,2,1,6,3,4,0), , ncol = 3) ) # - creates the matrix
##   cacheSolve(myMatrix) # - Calculates the inverse and caches it
##   cacheSolve(myMatrix) # - The following calls uses the cached version of the result

## Creates a matrix whose inverse calculation can be cached. 
## The argument must be an invertible matrix.
## To get the data from the matrix, call the get method.
## To change data, call the set method. 
## The getinverse should not be called directly, instead call the cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
            iv <- NULL
            set <- function(y) {
                    x <<- y
                    iv <<- NULL
            }
            get <- function() x
            setinverse <- function(inverse) iv <<- inverse
            getinverse <- function() iv
            list(set = set, get = get,
                 setinverse = setinverse,
                 getinverse = getinverse)
}


## Calculates the inverse of a matrix created through the "makeCacheMatrix" function.
## If the inverse had already been calculed, the cached version is returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        iv <- x$getinverse()
        if(!is.null(iv)) {
                message("getting cached matrix")
                return(iv)
        }
        data <- x$get()
        iv <- solve(data, ...)
        x$setinverse(iv)
        iv
}

## These functions create a special matrix; caches and returns the inverse of that matrix.

## This function creates a special matrix and caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
        vers <- NULL
                set <- function(y){
                x <<- y
                vers <<- NULL
        }
         get <- function() x
        setinverse <- function(solve) vers <<- solve
        getinverse <- function() vers
        list(set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse)

}


## This function returns the inverse of the special matrix, provided the matrix is square and invertible.  
## If the inverse is stored in the cache, then it returns the cached inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        vers <- x$getinverse()
        if(!is.null(vers)){
                message("getting cached data")
                 return(vers)
        }
        data <- x$get()
        vers <- solve(data, ...)
        x$setinverse(vers)
        vers
}

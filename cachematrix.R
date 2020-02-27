## The following two functions take a matrix to calculate the inverse.
## Since computing large data consumes time, these functions store the results in a cache.
## If the inverse has been calculated previously, that value is retrieved instead of calculating again.





## makeCacheMatrix creates a "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y){
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinv <- function(inv_value) inverse <<- inv_value
        getinv <- function() inverse
        list(set = set,
             get = get,
             setinv = setinv,
             getinv = getinv)

}




## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated, then cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        inverse <- x$getinv()
        if (!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        mat <- x$get()
        inverse <- solve(mat, ...)
        x$setinv(inverse)
        inverse
        
        
}


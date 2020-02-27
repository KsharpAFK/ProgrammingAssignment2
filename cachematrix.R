## The following two functions take a matrix to calculate the inverse.
## Since computing large data consumes time, these functions store the results in a cache.
## If the inverse has been calculated previously, that value is retrieved instead of calculating again.





## makeCacheMatrix creates a "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        ## initialize empty matrix
        inverse <- NULL 
        
        ## set the matrix
        set <- function(y){                 
                x <<- y
                inverse <<- NULL
        }
        
        #get the matrix
        get <- function() x
        
        ## assign the inverse value to the empty matrix
        setinv <- function(inv_value) inverse <<- inv_value 
        
        ## returns the inverse 
        getinv <- function() inverse
        
        ## store the results in a list
        list(set = set,
             get = get,
             setinv = setinv,
             getinv = getinv)

}




## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated, then cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        inverse <- x$getinv()
        
        ## check if inverse already stored
        if (!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        
        ## get the matrix
        mat <- x$get()
        
        ## solve the matrix
        inverse <- solve(mat, ...)
        
        ## store the value in the cache
        x$setinv(inverse)
        
        inverse
        
        
}


## The following functions carry out a matrix inverse caching by creating a special object
## which stores a matrix and its inverse.

## The function makeCacheMatrix creates a special object (list) which contains some functions for
## caching. The input is an invertable matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## The function cacheSolve calculates the inverse of the special matrix which is created using the 
## makeCacheMatrix function. If the inversion of the matrix has already been done then the result 
## is returned from the cache (getinv function). Otherwise - the inverse is calculated and stored 
## in the cache (setinv function).

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}


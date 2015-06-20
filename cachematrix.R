## Consists of two functions, one that creates a special matrix object capable
##      of caching its inverse, if the inverse was calculated, and the other that
##      does the actual inverse calculation, and caching.
##

## makeCacheMatrix : Creates a special Matrix object (list) which consists of
##       a regular matrix variable, a cached inverse variable, and a few helper
##       functions.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse
             )
}


# cacheSolve : When passed the special matrix object defined above, it calculates
#       the inverse of the matrix and caches it in the matrix object, if it has not
#       been calculated before. If it has, it will return the cached value.
#

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("Getting cached data.")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

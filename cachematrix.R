## The following functions compute the inverse of a given matrix
## and cache them, allowing them to simply be recalled instead of 
## being fully recomputed.  This is can save significant amounts
## of time, especially when running loops.

## This function creates a special "matrix" object that can cache its inverse.
## It does this in 4 steps- setting the value of the matrix, getting the value,
## then setting the value of the inverse, and getting the inverse.

makeCacheMatrix <- function(x = matrix()) {
    t <- NULL
    set <- function(y) {
        x <<- y
        t <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) t <<-inverse
    getInverse <- function() t
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        t <- x$getInverse()
    if ( ! is.null(t)) {
        print("getting cached data")
        return(t)
    }
    t <- solve(x$get())
    x$setInverse(t)
    t
}
}

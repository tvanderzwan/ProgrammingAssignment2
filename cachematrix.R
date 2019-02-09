## Together, makeCacheMatrix and cacheSolve create a matrix, compute/cache 
## the inverse of said matrix, and print the inverse matrix. If the inverse of 
## the matrix was previously computed, the inverse of the matrix is obtained 
## from the cache. If it was not previously computed, the inverse of the 
## matrix is computed and set in the cache using the setsolve function. In so 
## doing, these functions together prevent unnecessary computation of the 
## inverse of a matrix if it had previously been computed.

## The makeCacheMatrix function creates a matrix, gets the value of the matrix, 
## sets the inverse of the matrix (using the solve function; setsolve), and 
## gets the inverse of the matrix (getsolve). 

makeCacheMatrix <- function(x = matrix()) {
                i <- NULL
                set <- function(y) {
                        x <<- y
                        i <<- NULL
                }
                get <- function() x
                setsolve <- function(solve) i <<- solve
                getsolve <- function() i
                list(set = set, get = get, 
                     setsolve = setsolve, 
                     getsolve = getsolve)
}


## The cacheSolve function solves the inverse of the matrix created by
## makeCacheMatrix. But prior to doing so, it first checks if 'i' (the inverse
## of the matrix) is not null (i.e., if the inverse had previously been 
## computed and stored in the cache). If so, the inverse of the matrix is 
## retrieved from the cache. If not, a matrix that is the inverse of matrix 'x'
## is solved using the solve function.

cacheSolve <- function(x, ...) {
                i <- x$getsolve()
                if(!is.null(i)) {
                        message("getting cached data")
                        return(i)
                }
                data <- x$get()
                i <- solve(data, ...)
                x$setsolve(i)
                i
}

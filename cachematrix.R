## This is a set of two functions. The first takes a basic matrix and attaches it to a number of functions in a cache as well as a value i which is its inverse (which is undefined initially).
## The second function retrieves the value i from the cached matrix and if it is underfined, calculates the inverse and then defines it for future calculations so it need not be calculated again.

## This creates a cached matrix which is actually a list of functions, the first of which sets the matrix, the second of which returns the matrix, and the third and fourth whic set and return the matrix respectively.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
}


## This function creates an environment level above the makeCacheMatrix function by acting on it, using the functions within makeCacheMatrix to first check if the inverse has been calculated and if so return it without calculating and if not it calculates and sets it for the future.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

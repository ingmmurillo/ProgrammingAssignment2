##This functions allows to cache a matrix
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    #Function that allows to set a cached matrix
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    ##Function that allow to get the matrix
    get <- function() x
    ##Sets the inverse function
    setInverse <- function(inv) inverse <<- inv
    ##
    getInverse <- function() inverse
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

##Functions that calculates the inverse matrix using a cached matrix
cacheSolve <- function(x, ...) {    
    inverse <- x$getInverse()
    ##Returns the matrix if it is cached
    if (!is.null(inverse)) {
        return(inverse)
    }
    ##Gets the cached data
    data <- x$get()
    ##Calculates the inverse matrix using the solve() function of R
    inverse <- solve(data, ...)
    x$setInverse(inverse)
    inverse
}
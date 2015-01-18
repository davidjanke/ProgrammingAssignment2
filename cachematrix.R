## makeCacheMatrix creates a list (this is basically what an object is in OOP languages)
## that contains a matrix and it's inverted form. The invert will happen only once if
## inversion happens using the cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
    # holder object for the cached inverted matrix
    solvedMatrix <- NULL
    
    set <- function(y) {
        x <<- y
        solvedMatrix <<- NULL
    }
    
    ## get the original matrix
    get <- function() {
        x
    }
    
    ## set the inverse matrix for this
    setSolved <- function(inverse) {
        solvedMatrix <<- inverse
    }
    
    ## get the stored inverse matrix
    getSolved <- function() {
        solvedMatrix
    }
    
    ## return an object encapsulating the data and the methods
    list(set = set, get = get, setSolved = setSolved, getSolved = getSolved)
    
}


## cacheSolve will solve a quadratix matrix, but instead of always doing the inversion
## it will look for the cached version of that matrix first withing the object itself

cacheSolve <- function(x, ...) {
    ## look up the cached inverse
    inverse <- x$getSolved()
    
    ## if the cached version is not null, it means, the inversion did already happen
    ## this just return that (and inform the caller)
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    
    ## we arrive at this code path when there is no cached version yet
    ## so just fetch the original matrix
    data <- x$get()
    
    ## invert the matrix
    inverse <- solve(data, ...)
    
    ## set the inverse of the original as the cached inverse
    x$setSolved(inverse)
    
    ## and obviously return it
    inverse
}

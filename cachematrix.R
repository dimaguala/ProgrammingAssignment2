## Function to get/set a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    ## place holder for returned value
    inv <- NULL
    
    ## function to set the matrix value to passed matrix and its inverse to null
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    ## function to retrieve the currently stored matrix
    get <- function() x
    
    ## resets the inverse to the passed value
    setInv <- function(inverse) inv <<- inverse
    
    ## function that retrieves the currently stored inverse
    getInv <- function() inv
    
    ## returns a list of functions
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}

## function to get the inverse of a matrix
cacheSolve <- function(x, ...) {
    ## Return the inverse of 'x'
    inv <- x$getInv()
    
    ## if the inverse is stored in cache, return it 
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    ## get the matrix x
    data <- x$get()
    
    ## perform the inverse calculation
    inv <- solve(data, ...)
    
    ## store the inverse in cache
    x$setInv(inv)
    
    ## return the inverse
    inv
}

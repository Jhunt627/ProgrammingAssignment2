## Matrix inversion can be a costly computation and it can be beneficial
## to calculate the inverse once and thereafter read the inverse from a cache
## instead of repeatedly calculating the inverse.  These functions cache the 
## inverse of a matrix.

## makeCacheMatrix creates a list of four functions:

##1. set the value of the matrix based on input and set the value of the inverse to NULL
##2. get the value of the matrix
##3. set the value of the inverse based on input (does not do the matrix inversion)
##4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL           # Sets the inverse to NULL, as it has not been calculated yet
    set <- function(y) {  # This function sets the value of the matrix to the input 
        x <<- y           # and the value of the inverse to NULL  
        inv <<- NULL
    } 
    get <- function() x   # This function returns the matrix
    setinv <- function(sol) inv <<- sol # This function sets the inverse to the input value 'sol'
    getinv <- function() inv #This function returns the inverse, whether NULL or not
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv) # Returns a list of the four functions   
}

## cacheSolve returns the inverse of a matrix 'x'.  It first tries to read the cached inverse,
## but if the inverse has not been previously calculated, it then calculates the inverse
## and writes the inverse to the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()    # Reads cached value/status into temporary variable
    if(!is.null(inv)) {  # If the temporary variable is not null (the inverse has been calculated)
        message("Getting cached data")
        return(inv)      # Returns the cached value and exits the function
    }                    # If the temp variable is null, the function will continue
    data <- x$get()      # Gets the matrix values 
    inv <- solve(data, ...) # Calculates the inverse of the matrix
    x$setinv(inv)        # Sets the calculated inverse into the cache
    inv                  # Returns the calculated inverse 
}

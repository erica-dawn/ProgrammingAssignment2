## A pair of functions to cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.

## Initialize x as matrix, i to be used later in code
makeCacheMatrix <- function(x = matrix()) {     
    i <- NULL     
    
    ##Define "setter"
    set <- function(y) {
        x <<- y     ## Assign input to x in parent environment
        i <<- NULL  ## Reset any stored inverse
    }
    
    ##Define "getter" for x
    get <- function() x
    
    ## Define "setter" for inverse, assign to i in parent environment
    setinverse <- function(inverse) i <<- inverse
    
    ## Retrieve inverse from parent environment
    getinverse <- function() i
    
    ## Assign each function to list object and return to parent environment
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Determine if inverse of matrix x has been calculated and calculate only if
## necessary.

cacheSolve <- function(x, ...) {
    ## call getmean() on x
    i <- x$getinverse()
    
    ## determine if inverse has already been calculated, return inverse
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    ## Calculate inverse if not already cached
    data <- x$get()  ##Get input object
    i <- solve(data, ...)   ##Get inverse
    x$setinverse(i)  ##Save inverse
    i
}

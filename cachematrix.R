## It's a pair of functions 
## that cache the inverse of a matrix

## Example for usage from R-console:
##
## > A <- matrix(c(3,4,5,4),nrow=2)
## > cacheA <- makeCacheMatrix(A)
## > cacheI <- cacheSolve(cacheA)
## [1] "Calculating inverse matrix"
## > cacheI <- cacheSolve(cacheA)
## [1] "Getting inverse matrix from cache"

## The makeCacheMatrix creates a special "matrix", which is really a list 
## containing a function to:
##
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the inverse matrix
## - get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    
    inverse <- NULL
    
    set <- function(y){
        x <<- y
        inverse <<- NULL
    }
    
    get <- function() x
    
    set_inverse <- function(new_inverse) inverse <<- new_inverse
    
    get_inverse <- function() inverse
    
    list(set = set, get = get, 
         set_inverse = set_inverse, 
         get_inverse = get_inverse)
    
}

## The function casheSolve calculates the inverse matrix for the special "matrix"
## created with the function makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$get_inverse()
    if (!is.null(inverse)){
        print("Getting inverse matrix from cache")
        return(inverse)
    }
    
    print("Calculating inverse matrix")
    data <- x$get()
    inverse <- solve(data, ...)
    x$set_inverse(inverse)
    inverse
}

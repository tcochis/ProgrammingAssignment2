## The functions below are used to cache the inverse of a matrix, so that it does not 
##   have to be recalculated repeatedly. 
##              
##
## Function        - makeCacheMatrix
## Created         - 2/21/15
## Description     - creates function list used to cache Matrix inverse
## Input parameter - x = the matrix that you wish to calculate the inverse of
## Output          - a list (vector) containing the functions set, get, setInverse, GetInverse


makeCacheMatrix <- function(x = matrix()) {

        i <- NULL
        
##      define functions
        set <- function(y) {
                x<<- y
                i <<- NULL
        }
        
        get <- function() x
        
        setInverse <- function(inverse) i <<- inverse
        
        getInverse <- function() i
        
##      return vector of functions        
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Function    - cacheSolve 
## created     - 2/21/15
## Description - returns the inverse of a matrix.
## Input parameters - 
##         x =  a list returned from the makeCacheMatrix function.
##         (...) =  additional parameters for the solve() function (if needed).
## Output      - a matrix that is the inverse of x. 
##               (If the inverse is cached, then the cached value is returned, otherwise the 
##               inverse is calculated and then cached before it is returned.)

cacheSolve <- function(x, ...) {
        
##      check to see if inverse is cached
        i <- x$getInverse()        
        if(!is.null(i)) {
                message("inverse retreived from cache")
                return(i)
        }
        
##      not cached, so calculate inverse
        data <- x$get()
        i <- solve(data)
        x$setInverse(i)
        
        ## Return a matrix that is the inverse of 'x'
        i
}

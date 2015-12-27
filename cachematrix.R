## makeCacheMatrix creates a special matrix (list actually) which caches/stores its inverse.
## cacheSolve first checks whether the matrix has inverse in cache, if not then this function calculates the inverse 

## this function takes creates a null matrix x, required matrix value is assigned to this matrix and inverse
## is calculated in four steps
## 1. set value of matrix 
## 2. get value of matrix
## 3. set value of inverse of the matrix 
## 4. get value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    invr <- NULL
    set <- function(y) {                     # matrix for which inverse has to be calculated is assigned to x 
        x <<- y                              # invr is the INVERSE of the required MATRIX    
        invr <<- NULL                        # initially inverse is set to NULL        
    }
    get <- function() x                      # this fucntion gets the value of x, simple one line fucntion
    setInvr <- function(Invr) invr <<- Invr   
    getInvr <- function() invr     
    list(set = set, get = get,               # this list contains four functions 
         setInvr = setInvr,
         getInvr = getInvr)                
}


## This function takes the special matrix as the argument and calulates its inverse, if it has not beee calculated
## before. If calculated before it takes the INVERSE of the matrix from the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invr <- x$getInvr()
    if(!is.null(invr)) {                     # if loop to check whether inveres has been calculated or not
        message("getting cached data")       
        return(invr)
    }
    matrix_1 <- x$get()                      # gets the matrix from the previous function and assign to matrix matrix_1
    invr <- solve(matrix_1, ...)             # if inverse is not calculated then it calculates using solve(X) function
    x$setInvr(invr)
    invr
}
################################
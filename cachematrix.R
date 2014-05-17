# Below are 3 functions:
# makeCacheMatrix - helps chaching an inverted matrix
# cacheSolve - finds an inverted matrix if never calculted, get the old from cache
# checkFUN - tests the other 2 functions behavior in action
## invmat - is a variable and a shortening that stands for Inverted Matrix

## makeCacheMatrix returns 4 subfunctions:
## set - sets a matrix to be inverted
## get - returns the matrix that was set
## setinvmat - caches an inverted matrix
## getinvmar - returns the cached inverted matrix

makeCacheMatrix <- function(x = matrix()) {        
        invmat <- NULL
        set <- function(y){
                x <<- y
                invmat <<- NULL
        }
        get <- function() x
        setinvmat <- function(solvedmat) {
                invmat <<- solvedmat
        }
        getinvmat <- function() invmat
        
        list(set = set, get = get, setinvmat = setinvmat, getinvmat = getinvmat)
}


## cacheSolve returns a matrix that is the inverse of 'x'
## Prints messages if the InvMat was calculated or cached for better debugging

cacheSolve <- function(x, ...) {        
        invmat <- x$getinvmat()
        if(!is.null(invmat)) {
                message("got invmat from cache")
                return(invmat)
        }
        data <- x$get()
        invmat <- solve(data, ...)
        message("calculated invmat")
        x$setinvmat(invmat)
        invmat
}

## checkFUN checks if other 2 functions are working correctly

checkFUN <- function() {
        ##Setting up functions to work with matrix
        x <- makeCacheMatrix()
        
        ##Setting up a square matrix
        y <- matrix(1:4, 2, 2)
        x$set(y)
        print(cacheSolve(x)) ##Should calculate invmat
        print(cacheSolve(x)) ##Should retrieve invmat from cache
        
        ##Checking what happens if the matrix is changed
        y <- matrix(rnorm(16), 4, 4) 
        x$set(y) 
        print(cacheSolve(x)) ##Should calculate invmat
        cacheSolve(x) ##Should retrieve invmat from cache
}
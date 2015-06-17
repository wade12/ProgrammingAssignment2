## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly.
## These 2 functions cache the inverse of a matrix.

## makeCacheMatrix:
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function( x = matrix() ) {
        
        invert <- NULL
        
        set <- function(y) {
                x <<- y
                invert <<- NULL
        } ## end set
        
        get <- function() x
        
        setinverse <- function(inverse) invert <<- inverse
        getinverse <- function() invert
        
        list( set = set, get = get, setinverse = setinverse, getinverse = getinverse )
        
} ## end function makeCacheMatrix



## cacheSolve:
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        invert <- x$getinverse()
        
        if(!is.null(invert)) {
                message("retrieving cached data")
                return(invert)
        } ## end if
        
        data <- x$get()
        invertt <- solve(data)
        x$setinverse(invert)
        
        invert
        
} ## end function cacheSolve


## test with sample matrix (Fibonacci Sequence)
M = matrix(c(0, 1, 2, 3, 5, 8, 13, 21, 34), nrow = 3, ncol = 3)
## print matrix
M
## run 1st Function
W = makeCacheMatrix(M)
## run 2nd Function
cacheSolve (W)


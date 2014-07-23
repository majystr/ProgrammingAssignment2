## cachematrix.R: Written for rprog-005.  These functions create an object that
## can cache the inverse of a matrix. Program is based on the makeVector and cacheMean
## per the assignment instruction's examples

## Usage: First, run "x_fun <- makeCacheMatrix()".  Then run "x_fun$set(matrix(1:4,nrow=2,ncol=2))"
## to store the matrix. To retrieve inverse, run "cacheSolve(x_fun)"

## makeCacheMatrix: creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmatrix <- function(solve) m <<- solve
    getmatrix <- function() m
    list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
}


## cacheSolve: computes the inverse of the special "matrix" returned by 
## makeCacheMatrix.  If the inverse has already been calculated (and the matrix
## has not changed), then cacheSolve retrieves from the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getmatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    m
}

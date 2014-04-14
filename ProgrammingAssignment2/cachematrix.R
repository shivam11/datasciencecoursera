## Functions to create a special matrix which can cache the value of its inverse
## and also function to calculate the inverse of a matrix if it does not already
## exist

## The makeCacheMatrix function is a special matrix
## which can cache the inverse of itself and thus decrease processing
makeCacheMatrix <- function(x = matrix()) {
    ## set the matrix inverse to null
    mInverse <- NULL
    ## inline function to set the value of the matrix
    set <- function(m) {
        ## set the matrix value
        x <<- m
        ## set the matrix inverse to null
        mInverse <<- NULL
    }
    ## inline function to retrieve the matrix
    get <- function() x
    ## inline function to set the value for the inverse of the matrix
    setinverse <- function(inverse) mInverse <<- inverse
    ## inline function to retrieve the value of the inverse of the matrix
    getinverse <- function() mInverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## function to calculate the inverse of the matrix it it does not have an 
## inverse

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    ## check if the inverse is not null
    if(!is.null(inverse)) {
        message("getting cached matrix")
        ## return the value of the inverse and exit the function
        return(inverse)
    }
    ## in case inverse is null get the value of the matrix
    data <- x$get()
    ## calculate the inverse of the matrix
    inverse <- solve(data, ...)
    ## set the value of the inverse
    x$setinverse(inverse)
    ## return the value of the inverse
    inverse
}
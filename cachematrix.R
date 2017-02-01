## Put comments here that give an overall description of what your
## functions do
## This script has two functions which basically cache a matrix inverse, namely
## "makeCacheMatrix" and "cacheSolve".
## Individual descriptions of functions are given below.

## Write a short comment describing this function
## The "makeCachematrix" creates a list of functions, which are further used in 
## "cacheSolve". The input to "makeCachematrix" should be a square matrix.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## Write a short comment describing this function
## The input to the function "cacheSolve" should be a "makeCachematrix" object.
## This function computes the inverse of a matrix if it has not been already 
## calculated before.
## If the inverse of the matrix has alrady been calculated, the function returns
## cached inverse of the matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data,...)
    x$setinverse(i)
    return(i)
}

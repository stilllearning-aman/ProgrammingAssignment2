## Put comments here that give an overall description of what your
## functions do. I have two funcitons makeCacheMatrix() and cacheSolve(). These
## functions take input as matrix and checks if the inverse is available in 
## cache memory otherwise calculates the inverse of matrix.

## Write a short comment describing this function
## makeCacheMtrix is a function which takes an input matrix variable.
## The variable x can also be assigned a new value using set() function in
## in the local environment. get() function helps to check the value of input
## variable x.
## setinv() allows the user to store the value of inverse of matrix x in local 
## environment. getinv() allows to check the value of inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
    m <<- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    
    setinv <- function(inv) m <- inv
    
    getinv <- function() m
    
    list(set = set, get = get, setinv =  setinv, getinv = getinv)
}


## Write a short comment describing this function
## the funciton cacheSolve helps in calculating the inverse of matrix if
## if it's not available in the cached memory.
## When the function is called, it first checks whether the value of inverse
## is already available in makeCacheMatrix() function, and if it's not, the 
## funciton cacheSolve create the inverse of matrix.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if (!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
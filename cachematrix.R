## This is a package that can transform a normal matrix object into a cache matrix object and invert the matrix.
## The inverse will be cached in the cache matrix during the first time it is calculated, and the subsequent
## query for the inverse will retrive the data from the cache.

## makeCacheMatrix takes an argument of normal matrix type and generate an object of cache matrix type with
## the following member functions:
##    1. set: updates the data of the cache matrix with the input matrix
##    2. get: retrieve the data of normal matrix type from the cache matrix
##    3. setinverse: updates the inverse of the matrix
##    4. getinverse: retrieve the cached inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(i) inv <<- i
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve takes an argument of cache matrix type, checks whether the inverse has been cached,
## if so, just returns the cashed inverse from the cache matrix
## if there's no cached inverse, calulate the inverse for normal matrix which is retrieved
## from the cache matrix.

cacheSolve <- function(x, ...) {
    ## x is a cache matrix
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)){
        message("getting cached data")
        return (inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}

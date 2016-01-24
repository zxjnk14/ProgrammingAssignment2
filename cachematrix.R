## These functions return the inverse of a matrix in a more efficient way - by storing the
## inverse in the cache, functions can return the stored value rather than calculating the inverse
## again in case the inverse has already been calculated for a particular matrix. 

## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to

## set the value of the matrix
## get the value of the matrix
## set the value of the matrix
## get the value of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function returns the computed inverse matrix. If the inverse is already in the cache, 
## the inverse will be returned. Otherwise the inverse will be calculated and stored in the cache. 

cacheSolve <- function(x, ...) {
    inv1 <- x$getinverse()
    if(!is.null(inv1)) {
      message("getting cached data.")
      return(inv1)
    }
    data <- x$get()
    inv1 <- solve(data)
    x$setinverse(inv1)
    inv1
        ## Return a matrix that is the inverse of 'x'
}

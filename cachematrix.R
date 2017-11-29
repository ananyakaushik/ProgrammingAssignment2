## These functions cache the inverse of a matrix

## Create the matrix object

makeCacheMatrix <- function(x = matrix()) {
  #create objects and a set of functions      
  inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        #return the list of functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Compute and cache the inverse of the matrix or retrieve
## previously calculated inverse from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        #retreive previously calculated inverse from teh cache
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        #compute and cache the inverse
        inv <- solve(data, ...)
        x$setinverse(inv)
        #reutrn the inverse
        inv
}

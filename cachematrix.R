## Put comments here that give an overall description of what your
## functions do

## Function creates matrix object
## Matrix object functions return matrix value and inverse matrix value

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
          x <<- y
          m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Function either returnes cached object or creates new one
## try getting data from matrix object
## if there is no cached data, create new inverse value, store value on
## matrix object and return the value

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
          message("getting cached data")
          return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}

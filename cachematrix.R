## Author: Juan Lema
## Course: R Programming (rprog-009)
## Assignment: #2
## Description: Calculating the inverse of a matrix can be costly. The pair of functions below help reduce this cost by caching the inverse.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        ## clear previously calculated inverse
        i <- NULL
        ## setter and getter functions for matrix values
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        ## setter and getter functions for inverse values
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        ## return a list with setter and getters defined above
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve will retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Find out if inverse has been already calculated
        i <- x$getinverse()
        ## If already calculated then return current value
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        ## If inverse hasn't been calculated
        data <- x$get()
        ## Return a matrix that is the inverse of 'x'
        i <- solve(data, ...)
        ## Store cached inverse value
        x$setinverse(i)
        ## Display inverse value
        i
}

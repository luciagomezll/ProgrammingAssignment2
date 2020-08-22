## The following functions compute and cache the inverse of a square matrix

## `makeCacheMatrix`: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        k <- NULL
        set <- function(y) {
                x <<- y
                k <<- NULL
        }
        get <- function() x 
        setinverse <- function(solve) k <<- solve
        getinverse <- function() k
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## `cacheSolve`: This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        k <- x$getinverse()
        if(!is.null(k)) {
                message("getting cached data")
                return(k)
        }
        data <- x$get()
        k <- solve(data,...)
        x$setinverse(k)
        k
}

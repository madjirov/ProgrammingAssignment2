## makeCacheMatrix creates a special "matrix" object that can cache its inverse 
## and cacheSolve computes the inverse of the special "matrix". If the inverse 
## has already been calculated then the cachesolve retrieve the inverse from the cache.

## The first function, makeCacheMatrix creates a special "matrix", which stores a list 
## of functions - set,  get, setinverse, getinverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function () m
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)
}


## The second function cacheSolve computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
## has not changed), then the cachesolve should retrieve the inverse from the cache. 

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached inverse matrix")
                return(m)
        }
        invm <- x$get()
        m <- solve(invm, ...)
        x$setinverse(m)
        m
}

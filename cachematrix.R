## Evaluate and cache the inverse of a matrix 

## A function that creates a list of functions which set and get the orginal 
## matrix and also set and get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## A function that evaluates the inverse of the matrix but first checks if it 
## has already been evaluated, if so, it retrieves it from the cache and skips 
## computation

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinv(inv)
        inv
}

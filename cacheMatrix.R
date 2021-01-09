## Put comments here that give an overall description of what your
## functions do

## Creates a special matrix/list that sets the value of the matrix
                        #gets the value of the matrix
                        #sets the value of the inverse matrix
                        #gets the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        cache <- NULL
        set <- function(y) {
                x <<- y
                cache <-- NULL
        }
        get <-function() x
        setinverse <- function(inverse) cache <<- inverse
        getinv <- function() cache
        list(set = set, get = get, setinverse = setinverse, getinv = getinv)
}


## Calculate the inverse of the special matrix created using the function above
## Assume the matrix is always invertible (a square)
cacheSolve <- function(x, ...) {
        cache <- x$getinv()
        if(!is.null(cache)) {
                message("getting cached data.")
                return(cache)
        }
        data <- x$get()
        cache <- solve(data)
        x$setinverse(cache)
        cache
}

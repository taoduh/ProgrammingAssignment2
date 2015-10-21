# USAGE
#
######## load functions to environment
# source(".../cachematrix.R")
#
######## generate a sample matrix
# rawMatrix <- rbind(c(1, -1/4), c(-1/4, 1))
#
######## call makeCacheMatrix to create an object that can store a matrix and its inverse
# cachedMatrix <- makeCacheMatrix(rawMatrix)
#
######## call cacheSolve and note that the inverse is returned
# cacheSolve(cachedMatrix)
#
######## call cacheSolve again and note that a cache of the inverse is returned
# cacheSolve(cachedMatrix)
#
######## validate that the returned inverse is valid
# cacheSolve(cachedMatrix) %*% rawMatrix



## accept an incoming matrix and define the following list of operations
#
#### get/set -- the matrix data
#### setinverse -- calculate the matrix inverse using the solve function and store in m
#### getinverse -- return m (the calculated inverse)
#
# return those operations in a list

makeCacheMatrix <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## check whether the incoming matrix has already been solved
## if so, print a message and return that value
#
## if not, calculate the inverse using the solve function
## store the result in the cache (using setinverse) and return it

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

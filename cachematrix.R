## Functions that cache the inverse of a matrix so it can be looked up rather than recomputed

## makes "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() return(x)
        setInverse <- function(inverse) m <<- inverse
        getInverse <- function() return(m)
        return(list(set = set, get = get, setInverse = setInverse, getInverse = getInverse))
}


## computes inverse of "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setInverse(m)
        return(m)
}

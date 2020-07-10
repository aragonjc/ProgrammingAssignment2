##Caching Inverse of a Matrix

## Crates a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) i <<- inverse
	getInverse <- function() i
	list(set = set,get = get, setInverse=setInverse, getInverse = getInverse)
}


## compute the inverse of a matrix.
## If the inverse has alredy been calculated return the inverse from the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if(!is.null(i)) {
    	return(i)
    }
    data <- x$get()
    i <- solve(data,...)
    x$setInverse(i)
    return(i)
}

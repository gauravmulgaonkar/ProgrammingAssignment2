## The two functions are programmed to create a special matrix and cache
## its inverse.

## makeCacheMatrix function creates a matrix object, 
## it initially sets the inverse to null.
## Cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	matinv <- NULL
	set <- function(y){
			x <<- y
			matinv <<- NULL
			     }
	get <- function() x
	setinv <- function(inverse) matinv <<- inverse
	getinv <- function() matinv
	list(set = set, get = get,
	     setinv = setinv, getinv = getinv)
}


## This function calculates the inverse of the matrix created 
## by makeCacheMatrix. If the inverse is already calculated
## then it returns the matrix inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	matinv <- x$getinv()
	if(!is.null(matinv))
	{
			message("getting cached data")
			return(matinv)
	}
	data <- x$get()
	matinv <- solve(data,...)
	x$setinv(matinv)
	matinv
}

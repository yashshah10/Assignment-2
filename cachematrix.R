## The makeCacheMatrix function takes the matrix from the user.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## the cacheSolve function calculates the inverse of the matrix and sets the inverse of the matrix and if the inverse is already present for that particular matrix, it retrieves from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
	if(!is.null(inv)) {
		message("Getting the cached data.....")
		return(inv)
	}
	mat_data <- x$get()
	inv <- solve(mat_data, ...)
	x$setinverse(inv)
	inv
}

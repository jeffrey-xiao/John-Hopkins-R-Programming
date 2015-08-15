# Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
	# define the cache for the inverse to be "inverse"
	inverse <- NULL
	# function that assigns a new matrix
	set <- function (y) {
		x <<- y
		inverse <<- NULL
	}
	# function that retrieves the current matrix associated with the special matrix object
	get <- function () {
		x
	}
	# function that sets the inverse of the current matrix
	setinverse <- function (inv) {
		inverse <<- inv
	}
	# function that retrieves the inverse of the current matrix
	getinverse <- function () {
		inverse
	}
	# return a list of the four aforementioned functions -- this will act as our special matrix object
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


# Computes the inverse of the special matrix returned by makeCacheMatrix.
# If the inverse has already been calculated and the matrix hasn't changed
# then the cachSolve function will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
	inverse <- x$getinverse()
	# checks if there is a cached value of the inverse
	if (!is.null(inverse)) {
		message("CACHE")
		return(inverse)
	}
	# if there is not, then compute the inverse, set it, and return it
	data <- x$get()
	inverse <- solve(data, ...)
	x$setinverse(inverse)
	inverse
}
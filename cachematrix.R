## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## create a matrix object and give a set of functions
makeCacheMatrix <- function(x = matrix()) {
	InverseMatrix <- NULL
	set <- function(y) {
		x <<- y
		InverseMatrix <<- NULL
	}
	get <- function() x
	setInverse <- function(xInverse) InverseMatrix <<- xInverse
	getInverse <- function() InverseMatrix
	list(set = set, get = get, 
		setInverse = setInverse,
		getInverse = getInverse) 
}


## Write a short comment describing this function
## compute matrix inverse from the make function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	InverseMatrix <- x$getInverse()
	if (!is.null(InverseMatrix)) {
		message("getting cached inverse")
		return(InverseMatrix)
	}
	matrix <- x$get()
	InverseMatrix <- solve(matrix,...) # not solve(x) as x is a list
	x$setInverse(InverseMatrix)
	InverseMatrix
}

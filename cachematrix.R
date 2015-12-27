## Coursera training R Programming (JHSPH)
## Assignment 2

## These functions calculate the matrix inverse and store it
## for future recall. If the inverse already exists, the
## cached version is returned.

makeCacheMatrix <- function(x = matrix()) {
## get input matrix dims to create NULL version of matrix inverse w/correct dims
	xdim <- dim(x)
	xrows <- xdim[1]
	xcols <- xdim[2]
## create NULL/NA version of matrix inverse w/correct dims
	v <- matrix(nrow = xrows, ncol = xcols)
	set <- function(y) {
		x <<- y
		v <<- matrix(nrow = xrows, ncol = xcols)
	}
	get <- function() x
	setinvs <- function(invs) v <<- invs
	getinvs <- function() v
	list(set = set,
		get = get,
		setinvs = setinvs,
		getinvs = getinvs)
}

## This function takes arguments including the list returned by makeCacheMatrix
## and computes the matrix inverse. It returns either the cached inverse (if it
## was previously computed) or the newly computed inverse. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	v <- x$getinvs()
	if(all(!is.na(v))) {
		message("getting cached data")
		return(v)
	}
	data <- x$get()
	v <- solve(data)
	x$setinvs(v)
	v
}

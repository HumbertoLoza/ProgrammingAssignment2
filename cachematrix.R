## This file has a couple of functions, makeCacheMatrix and cacheSolve
## They are built to help calculate the inverse of known matrices faster by keeping a cache of calculated elements

## This function receives a matrix as argument and it returns a list of 4 functions that can be called from 
## the main function: set, get, setInverse and getInverse
## set - sets the value of the matrix which inverse we want to calculate
## get - gets the matrix which inverse we want to calculate
## setInverse - sets the solution to the current matrix
## getInverse - gets the solution of the current matrix

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x<<-y
		m<<-NULL
	}
	get<-function() x
	setInverse<-function(inverse) m<<- inverse
	getInverse <- function() m
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) 
}


## This function receives a matrix created by the makeCacheMatrix function declared above ^
## It checks whether there is already a solution for the matrix or not
## If it already exists, the solution is returned, otherwise the solution is calculated using the solve function
## more information ?solve

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m<-x$getInverse()
	if (!is.null(m)) {
		return (m)
	}
	data <- x$get()
	m <- solve(data,...)
	x$setInverse(m)
	m
}


#Testing the functions
m<-matrix(c(1,4,6,8,2,-3,3,4,2),3,3)
make<-makeCacheMatrix(m)
#We want to know if the cache is actually being used
debug(cacheSolve)
cacheSolve(make)
cacheSolve(make)
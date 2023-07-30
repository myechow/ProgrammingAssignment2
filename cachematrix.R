## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}


makeCacheMatrix <- function(x = matrix(sample(1:100,9),3,3)) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setsolve <- function(solve) m <<- solve
	getsolve <- function() m
	list(set = set, get = get,
		 setmean = setmean,
		 getmean = getmean)
}


makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x 
	setinv <- function(inverse) inv <<- inverse
	getinv <- function() inv
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


cacheSolve <- function(x, ...) {
	m <- x$getmean()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- mean(data, ...)
	x$setmean(m)
	m
}
cacheSolve <- function(x, ...) {
	inv <- x$getinv()
	if(!is.null(inv)) {
		message("getting cached result")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinv(inv)
	inv
}

ma <- matrix(rnorm(16),4,4)
mn <- makeCacheMatrix(m)

cacheSolve(mn)


makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) inv <<- inverse
	getInverse <- function() inv
	list(set = set,
		 get = get,
		 setInverse = setInverse,
		 getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
	
	inv <- x$getInverse()
	if (!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	mat <- x$get()
	inv <- solve(mat, ...)
	x$setInverse(inv)
	inv
}

	my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
	my_matrix$get()
	
	my_matrix$getInverse()
	cacheSolve(my_matrix)
	
	cacheSolve(my_matrix)
	
	my_matrix$getInverse()
	
	my_matrix$set(matrix(c(2, 2, 1, 4), 2, 2))
	my_matrix$get()
	
	my_matrix$getInverse()
	
	cacheSolve(my_matrix)
	
	cacheSolve(my_matrix)
	
	my_matrix$getInverse()



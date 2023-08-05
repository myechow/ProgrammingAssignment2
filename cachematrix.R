
makeCacheMatrix <- function(x = matrix(sample(1:100,9),3,3)) {   ##setting the value of vector
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x                   			##getting the value of vector
	setInverse <- function(inverse) inv <<- inverse
	getInverse <- function() inv
	list(set = set,
		 get = get,
		 setInverse = setInverse,
		 getInverse = getInverse)
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

							##checking the program
	
		m <- matrix(rnorm(16),4,4)
		n <- makeCacheMatrix(m)

		cacheSolve(n)


## The two functions, makeCacheMatrix and cacheSolve, work in conjuction with each other
## The functions store a matrix in the cache
## Thie first time the function is called it calculates the inverse of the matrix
## usig the solve function and stores the value in the 'cache' or environment
## The second time the function is called, if the matrix has not chagned,
## it returns the inverse from the cache value instead of re-calculating the inverse

## This function assigns the value of a square invertible matrix to the value get
## and creates functions to assign the inverse to getinv once the cacheSolve function
## has been called

makeCacheMatrix <- function(x = matrix()) {

	## when we call the function makecacheMatrix
	## assign m as null since the inverse 
	## has not been calculated
	m <- NULL
	
	## create a function to assign values
	## to the get() and getinv()
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinv <- function(solve) m <<- solve
	getinv <- function() m
	
	## assign the values to a list
	list(set=set, get=get,
		setinv=setinv,
		getinv=getinv)

}


## This function either determines the inverse of the matrix
## or if the inverse has already been calculated 
## the function returns the cached value of the inverse

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    	## assign m the value of $getinv
		m <- x$getinv()
	 
			## if m is not null (indicating the inverse has been calculated)
			## then
			## 1. print to the console message getting cached data
			## 2. return the value of m which was already calculated
			if(!is.null(m)) {
				message("getting cached matrix data")
				return(m)
			}
	
			## else if m is null
	
			## assign to data the value of the original matrix
				data <- x$get()
	
			## then determine the inverse of the matrix
			## using the solve function
				m <- solve(data, ...)
	
			## next assign the value of m to the inverse
				x$setinv(m)
	
			## and return the inverse to the console
				m

}

## The makeCacheMatrix fucntion creates a special matrix that caches its inverse matrix.
##The cacheSolve function computes the inverse of the matrix returned by the makeCacheMatrix function. 


## makeCacheMatrix function
## This function has 4 sub functions set, get, setinverse, getinverse. Set function changes the matrix stored in the main function with the matrix sent as an argument.
## Get function retunrs the matrix stored. Setinverse function sets the inverse of the matrix to be the matrix passed as argument.Getinverse function returns the inverse of the current matrix

makeCacheMatrix <- function(x = matrix()) {
	inverse_x <- matrix()
	set <- function(y) {
		x <<- y
		inverse_x <<- matrix()
	}
	get <- function() x
	setinverse <- function(inverse) inverse_x <<- inverse
	getinverse <- function() inverse_x
	list(set = set, get = get,
	setinverse = setinverse,
	getinverse = getinverse)
}


##cacheSolve function
## This function returns the inverse of the matrix. If the inverse is already computed, it displays a message and retrieves the inverse matrix from the cache. If the inverse is not yet computed then it gets the matrix by calling the get()  function and then computes the inverse using the inbuilt solve function and then calls the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse_x <- x$getinverse()
        if(!all(is.na(inverse_x))) {
                message("geting cached data")
           	    return(inverse_x)
        }
        mat <- x$get()
        inverse_x <- solve(mat, ...)
        x$setinverse(inverse_x)
        inverse_x
}

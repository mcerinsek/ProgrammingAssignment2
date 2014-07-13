## The two functions creates a list that stores an
## inversible matrix and cache's its inverse.


## The function makeCacheMatrix reads an inversible
## matrix and creates a list of four functions:
## for setting the value of the matrix, for getting 
## the value of the matrix, for setting the value
## of the inverse, and for getting the value of 
## the inverse.

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
  	set <- function(y) {
    	x <<- y
    	i <<- NULL
  	}
  	get <- function() x
  	# inverse: solve(c)%*%c for c a matrix
  	setinverse <- function(inverse) i <<- inverse
  	getinverse <- function() i
  	list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The function cacheSolve returns the inverse of
## given matrix.
## If the inverse is cache it just returns it,
## otherwise it calculates it and stores it value
## in the list that is calculated in the function
## makeCacheMatrix.

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
  	if(!is.null(i)) {
    	message("getting cached data")
    	return(i)
  	}
  	data <- x$get()
  	i <- solve(data)%*%data
  	x$setinverse(i)
  	print(i)
}

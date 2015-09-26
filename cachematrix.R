## Put comments here that give an overall description of what your
## functions do

## Creates a special "matrix"

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL

	## Set new matrix, nullify cache
	set <- function(y) {
		x <<- y
		i <<- NULL
	}

	## Get matrix
	get <- function() x

	## Set inverse for current matrix
	setinverse <- function(inv) i <<- inv

	## Get inverse for current matrix
	getinverse <- function() i

	## Return a list object containing functions
	list(set = set, get = get,
		setinverse = setinverse ,
		getinverse = getinverse )
}


## Calculates inverse for special "matrix" with caching

cacheSolve <- function(x, ...) {
      ## Get inverse
	inv <- x$getinverse()

	
	if(!is.null(inv)) {
		## Inform user of use of cached data
		message("getting cached inverse")
		
		## Return inverse if it does exist
		return(inv)
	}

	## Get matrix
	mat <- x$get()

	## Calculate its inverse
	inv <- solve(mat)

	## Cache the inverse of a matrix
	x$setinverse(inv)

	## Returns a matrix that is the inverse of 'x'
	inv
}

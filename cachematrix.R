## 2019.09.14 Alok Yadav R Programming Week 3 Assigment
## Function will perform the Matrix inversion and caching the inverse of a matrix rather than compute it repeatedly


## 1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	input_inverse <- NULL
	
	set <- function(y){
		x <<- y
		input_inverse <<- NULL
	}
	
	get <- function() x
	set_inverse <- function(inverse) input_inverse <<- inverse
	
	get_inverse <- function() input_inverse
	
	list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)	

}

## 2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## 	  If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		input_inverse <- x$get_inverse()
		
		if(!is.null(input_inverse)){
				message("getting cached data")
				return(input_inverse)
			}
			
		input_mat <- x$get()
		
		input_inverse <- solve(input_mat, ...)
		
		x$set_inverse(input_inverse)
		input_inverse
}

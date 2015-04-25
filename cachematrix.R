## Programming Assignment 2: Lexical Scoping
## Using scoping rules of the R language to preserve state inside of an R object.
##
## Matrix inversion is usually a costly computation the following function is 
## to cache the inverse of a matrix rather than computing it repeatedly 
## It can be looked up in the cache rather than recomputed. 
##
## Two functions are created : makeCacheMatrix and cacheSolve 
##
## makeCacheMatrix : A function to create a special "matrix" object that
## can cache its inverse.
##
## cacheSolve : This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then cacheSolve should 
## retrieve the inverse from the cache.





## The funtion makeCacheMatrix creates a special "Matrix" which is  
## a list of the following functions
## setMatrix - set the value of the Matrix
## getMatrix - the value of the Matrix
## setInverse - set the value of the Inverse of the Matrix
## getInverse - get the value of the Inverse of the Matrix
## The <<- operator is used to assign a value to an object in an environment 
## that is different from the current environment.
makeCacheMatrix <- function(x = matrix()) {
 		  
                m <- NULL                       # initialize an variable in cache as NULL

	        # set the Matrix inputed in cache
        	setMatrix <- function(y) {
              		 	x <<- y         # assign the matrix with the input value
	                   	m <<- NULL      # the matrix already set a value, clean the cache 
        	             }	

 		# get the value of matrix	
	        getMatrix <- function() {
                                x               # return the matrix  
                             }	
 
		# set the value of inverse of the matrix in cache
		setInverse <- function(inverse) {
				m <<- inverse   # assign the value in cache from calculated inverse
			     }	

		# get the value of inverse from cache
		getInverse <- function() {
                                m               # return the value of inverse in cache
                             }

		 # return a list of functions
                list(setMatrix = setMatrix , getMatrix = getMatrix, setInverse=setInverse, getInverse=getInverse )
}









## The function cacheSolve calculates the inverse of the special "Matrix" created
## with the above function. It first checks if the mean has already been calculated. 
## If yes, it gets the inverse from the cache. Otherwise, it calculates the inverse 
## of the matrix and sets the value in cache using the setInverse function

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
                     m <- x$getInverse()           # get the cache value 
          	     if(!is.null(m)) {
            	            message("getting cached data")
                	    return(m)              # return the cache value if the inverse value is already there, and exit the function
                        }

		
	              data <- x$getMatrix()        # Otherwise, get the matrix and calculate the inverse
                      m    <- solve(data, ...)
                      x$setInverse(m)              # assign the value of inverse in cache
                      m                            # return the inverse
}
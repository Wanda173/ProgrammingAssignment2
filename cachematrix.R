## Programming Assignment 2 - 26/08/2020
##
## The purpose of the functions is to cache the inverse of a matrix.
## The input matrix is assumed to be always invertible.
## makeCacheMatrix function will create the matrix object and its inverse.
## cacheSolve will use the argument returned by makeCacheMatrix to get  
## the inverse from the cache or add the inverse matrix in the cache if it does not exist.
##
## Example of how to call : 
## m <- makeCacheMatrix ( matrix(1:4,2,2))
## cacheSolve(m)


## This function creates a special "matrix" object x that can cache its inverse.
## When this function is called, it builds the set, get, setinverse and getinverse functions and 
## the x and i data objects within a parent environment.
##
makeCacheMatrix <- function(x = matrix()) {
    # initialise inverse variable to null 
    i <- NULL                    
	# function to overwrite variable x in the parent environment with y and reset the inverse i in the parent environment
    set <- function(y) {			   
        x <<- y
        i <<- NULL
    }
	# function to retrieve variable x
    get <- function() x
	# function to store inverse in i in the parent environment
    setinverse <- function(inverse) i <<- inverse
	# function to get the inverse from variable i
    getinverse <- function()  i
	# used to allow x$names to be used by name
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix function. 
## If the inverse has already been calculated and the matrix has not changed, 
## then the cacheSolve fetches the inverse from the cache.
##
## When a new matrix x value is passed (E.g m <- makeCacheMatrix ( matrix(1:4,2,2)), 
## the x function argument is initialised and the global inverse variable is null.
## Upon calling cacheSolve(m), the inverse of x is stored in the cache in the parent environment as it does not exist.
## When calling cacheSolve(m) again, the global inverse variable has already been set and the value of the inverse is 
## directly fetched from the cache i object variable.

cacheSolve <- function(x, ...) {
    # Call getinverse to fetch inverse if exists
    i <- x$getinverse()
	# if inverse exist, display the inverse and stop here
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
	# else retrieve the x matrix value input 
    data <- x$get()   
    # calculate the inverse of the matrix 
    i <- solve(data, ...)
	# store the value of the inverse matrix to the global parent environment inverser object variable
     x$setinverse(i)
	# display inverse matrix 
    i
}

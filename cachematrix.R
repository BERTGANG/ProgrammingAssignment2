## Put comments here that give an overall description of what your
## functions do

# the function makeCacheMatrix() can create a matrix and can cache its inverse, besides this it 
# offers functions for getting and setting the matrix and for getting and setting the cashed inverse matrix
# the function cacheSolve() calculates the inverse matrix of a given matrix and caches the inverse matrix

## Write a short comment describing this function

# makeCacheMatrix() is a function, that has 4 functions to operate with 
# a matrix. These function are: get() -> to get the current assigned matrix
# set() -> to set a new matrix, but only if the new matrix differs from
# the already assigned matrix; getcache() -> returns the cached inverse matrix;
# setcache() -> sets the new calculated inverse matrix

makeCacheMatrix <- function(x = matrix()) {

        # Inversed matrix is set to NULL, forcing a 
        # new calculation when calling function cacheSolve()
        inverse_matrix <- NULL
        
        # function to assign a new matrix
        set <- function(y) {
                
        # only if a matrix diffent from the already 
        # existing matrix shell be set then variale x is assigned to 
        # the new matrix and the cached inverse matrix is set
        # to NULL in order to force a new calculation when 
        # calling function cacheSolve()
                
                if (!identical(x,y)) {
                x <<- y
                inverse_matrix <<- NULL
                } 
                
        }
        
        # returns the actual assigned matrix
        get <- function() {
                x
        }
        
        # sets the new calculated inverse matrix, 
        # called by function cacheSolve()
        setcache <- function(inv_mat) { 
                inverse_matrix <<- inv_mat 
        }
        
        # returns the current cached inverse matrix
        getcache <- function() { 
                inverse_matrix 
        }
        
        # creates a list of available functions for the object
        # created by function makeCacheMatrix()
        list(set = set, get = get,
             setcache = setcache,
             getcache = getcache)

}


## Write a short comment describing this function

# cacheSolve() is a function that calculates an inverse
# matrix of a given matrix and caches the inverse matrix

cacheSolve <- function(x, ...) {

        #  returns the current cached inverse matrix
        inverse_matrix <- x$getcache()
        
        # Check if cached matrix is not NULL then a message is 
        # printed and the cached inverse matrix is returned
        if(!is.null(inverse_matrix)) {
                message("getting cached data")
                return(inverse_matrix)
                
        } else {
                
        # otherwise the calculation of the inverse matrix
        # starts         
                
        # therefore current assigned matrix is loaded to variable data        
        data <- x$get()
        
        # Calculation of the inverse matrix
        inverse_matrix <- solve(data, ...)
        
        # the value of the inverse matrix will be cached
        # and returned
        x$setcache(inverse_matrix)
        inverse_matrix
        }
}

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##This function creates a special "matrix" object that can cache ##its inverse.
makeCacheMatrix <- function(x = matrix()) {

##intialize the inverse function object and set method to set 
## the matrix

invrs <- NULL;
  set <- function(y){
    x <<- y
    invrs <<- NULL
  }
  
# this method get the matrix 
  get <- function()x
# this function will set the inverse of matrix
  set_inverse <- function(inverse) invrs <<- inverse
# this function will get the inverse of matrix
  get_inverse <- function() invrs

# list all the functions
  
  list(set = set, get = get, 
       set_inverse = set_inverse,
       get_inverse = get_inverse)

}


## this function will return the inverse of special matrix
##returned by "makeCacheMatrix" above.

cacheSolve <- function(x, ...) {

## Return a matrix that is the inverse of 'x'

invrs <- x$get_inverse()

## this will return the inverse if it is already calculated.
  if(!is.null(invrs))
  {
    message("getting Cahed data")
    return(invrs)
  }
  
##  this will get the matrix from the special object 
  data <- x$get()
## the solve method is used to return the inverse of matrix.
  invrs <- solve(data)
  x$set_inverse(invrs)
## return the inverse.
  invrs
  
}

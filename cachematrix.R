
# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix<-function(x=matrix())
{
  # Set to NULL initially
  inv<-NULL
  
  # Set the matrix 
  # Inverse is still NULL
  setmatrix<-function(new_x){
    x<<-new_x
    inv<<-NULL
  }
  
  # Get the matrix
  getmatrix<-function() x
  
  # Set the inverse
  setinverse<-function(inverse) inv<<-inverse
  
  # Get the inverse
  getinverse<-function() inv
  
  # Encapsulate into a list
  list(setmatrix=setmatrix,getmatrix=getmatrix,setinverse=setinverse,getinverse=getinverse)
  
}

# The following function calculates the inverse of the special "matrix"
# created with the above function. 
# However, it first checks to see if the inverse  has already been calculated.
# If so, it gets the inverse from the cache and skips the computation.
# Otherwise, it calculates the inverse of the data and sets the value 
# of the inverse in the cache via the setinverse function.




cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  # Following the same format as the assignment example
  
  # Get the current state of the inverse and see if it
  # has been computed yet
  inv <- x$getinverse()
  
  # if the value of inverse is chached
  if(!is.null(inv)) {
    #Simply return the computed inverse
    message("getting cached data")
    return(inv)
  }
  
  # otherwise,Get the matrix itself
  data <- x$getmatrix()
  
  # Find the inverse
  inv<- solve(data, ...)
  
  # Cache the value to object
  x$setinverse(inv)
  
  # Return result
  inv
}


## Usage example:
## x <- matrix(2:7, nrow=2, ncol=2)
## mat <- makeCacheMatrix(x)
## sol <- cacheSolve(mat)
## sol
## sol returns:
##          [,1] [,2]
##    [1,] -2.5    2
##    [2,]  1.5   -1
##
## sol2 <- cacheSolve(mat)
## This displays a "Getting cached matrix" message
## sol2
## sol2 returns:
##          [,1] [,2]
##    [1,] -2.5    2
##    [2,]  1.5   -1
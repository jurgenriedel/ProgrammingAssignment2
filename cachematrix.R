#####################################################################
## The function makeCacheMatrix creates creates a special "matrix", 
## which is a list containing a function to
## 1.	set the value of the matrix
## 2.	get the value of the matrix
## 3.	set the value of the inverse of the matrix
## 4.	get the value of the inverse of the matrix
## The matrix is assumed to be invertable
#####################################################################

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL 
  ## Define set function
  set <- function(y) {
    		x <<- y
    		inv <<- NULL
  }
  ## Define get function
  get <- function() x
  ## Implement setinverse method
  setinverse <- function(solve) inv <<- solve
  ## Implement getinverse method
  getinverse <- function() inv
  
  ## Return
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


###################################################################################
## function calculates the inverse of a matrix of the special "matrix" created with
## the above function makeCacheMatrix. First it checks to see if the iverse of the
## matrix has already been calculated. If so, it gets the inverse matrix from the
## cache and skips the computation. 
## Otherwise, it calculates the invere of the matrix and
## sets the value of the inverse in the cache via the setinverse function.
###################################################################################

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## Retrieve inverse
  inv <- x$getinverse()
  ## Check if it does exist. 
  if(!is.null(inv)) {
    		message("getting cached matrix")
    		## Retrun cahced inverse matrix.
    		return(inv)
  }
  ## Cache does not exist. Calculate the inverse.
  data <- x$get()
  inv <- solve(data, ...)
  ## Cache the result
  x$setinverse(inv)
  ## Return result
  inv
}

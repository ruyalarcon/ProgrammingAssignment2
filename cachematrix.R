## These two functions are used to create a special matrix object
## that stores a numeric matrix and caches its inverse.

## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to:

## set the value of the matrix
## get the value of the matrix
## set the inverse matrix
## get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    #set the initial value of matrix inverse in NULL
    i <- NULL
    
    #set function, sets a fresh value of the matrix
    #and resets the matrix inverse (the matrix has changed)
    set <- function(y = matrix()) {
      x <<- y
      i <<- NULL
    }
    
    #get function returns the matrix
    get <- function() x
    
    #setInverse function sets the parameter inv as the matrix
    #inverse
    setInverse <- function(inv) i <<- inv

    #getInverse function returns the cached inverse matrix
    getInverse <- function() i

    #return the list of functions supported by makeCacheMatrix   
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
  }


## The second function calculates the inverse of the special "matrix" 
## created with the makeCacheMatrix function. It calculates and sets the 
## value of the inverse matrix. If no inverse exists or the matrix has 
## changed (inverse is NULL in both cases). Otherwise it returns the cached
## inverse and skips the computation. 

cacheSolve <- function(x, ...) {
  # The function determines if there exists already an inverse
  # matrix stored for x (getInverse is not null), if so the 
  # cached matrix is returned
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  # Otherwise, the inverse matrix is calculated, stored and returned
  # First, the matrix is obtained
  m <- x$get()
  
  # Then the inverse matrix is calculated
  i <- solve(m, ...)
  
  # and stored
  x$setInverse(i)
  
  # the inverse matrix is returned
  i
}

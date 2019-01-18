## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

  inv <<- NULL                    # intitalise to NULL
  set <- function(y) {            # define  set function
    x <<- y                       # assign matrix to x in this environment
   inv <<- NULL                   # if there is a new matrix, reset inv to NULL
  }
  get <- function() x             # define get function - returns value of the matrix argument
  
  setInverse <- function(inverse) inv <<- inverse  # assigns value inverse to inv in parent environment
  getInverse <- function() inv                     # gets the value of inv where called
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)   
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {                # if inverse has be calculated already
    message("getting cached data")   # get it from cache, and alert
    return(inv)
  }
                                     # otherwise, calculate the inverse
  data <- x$get()
  inv <- solve(data, ...)             
  
  x$setInverse(inv)                  # set value for the inverse in cache 
  inv                                # return inv 
}

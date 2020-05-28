## Put comments here that give an overall description of what your
## functions do
## This is a programme to cache the inverse of a matrix
## Write a short comment describing this function

##This function creates the matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {## a function is created with the argument x having a default mode of a matrix 
    inv <- NULL                            ## inv is a variable that will hold the value of the inverse of the matrix, initialized as NULL
    set <- function(y) {                   ## a new function is defined here
      x <<-y                               ## new value of the matrix is assigned in the parent environment
      inv <<- NULL                         ## if there is a new matrix inv will reset to NULL
    }                       
    get <- function() x                    ## this function returns the value of the matrix argument
    
    setinverse <- function(inverse) inv <<- inverse    ##assigns value of inv in the parent environment
    getinverse <- function() inv                       ## gets the value of inverse when called
    list(set = set,get = get, setinverse = setinverse , getinverse = getinverse)  ## this helps us refer to the function with $ operator
}


## Write a short comment describing this function

## This function computes the inverse of the matrix returned by the above function
## I the matrix is not changed and inverse was already calculated then this function will retrive the inverse from the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}



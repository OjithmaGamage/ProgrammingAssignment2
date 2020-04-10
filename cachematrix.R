## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) { ##define the argument with the default mode of matrix
 inv <- NULL ##initialize inv as NULL;will hold value of matrix inverse
 set <- function(y){ ##define the set function to assign new
   x <<- y ##the value of the matrix in the parent environment
   inv <<- NULL ##if there is a new matrix, reset inv to null
 }
 get <-function()x ##define the get function ; returns the value of the matrix argument
 setinverse <- function(inverse) inv <<- inverse ##assigns the value of inv in the parent environment
 getinverse <- function() inv ##gets the value of inv where it is called
 list(set = set, get=get, setinverse= setinverse, getinverse= getinverse) ## need this in order to refer to the function with "$" operator
 
}


## Write a short comment describing this function
##This function computes the special "matrix" returned by the function above, makeCacheMatrix;
##If the inverse has been already calculated such that the matrix has not changed, then cacheSolve
## will retrieve the inverse from the cache.

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


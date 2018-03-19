## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#This function creates a special "matrix" object that can cache its inverse
## I will define the argument with a default mode of (Matrix)
## Initiliaze inv as NULL; so it will hold value of matrix inverse
## Then define the set function to assign new value of matrix in parent environment, ## if a new matrix then reset inv to NULL

makeCacheMatrix <- function(x = matrix()) {             
   inv <- NULL                                         
   set <- function(y) {                                
   x <<- y                                              
   inv <<- NULL                                         
}
         get <- function() x     ## define the get fucntion: returns value of the matrix argument
        
 ## assign value of inv in parent enrivonment
 ## get the value of inv where called
 ## you need this in order to refer to the functions with the $ operator
   
    setinverse <- function(inverse) inv <<- inverse  
    getinverse <- function() inv                     
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  
                                                                                
 }

## This function computes the inverse of the special (matrix) returned by makeCacheMatrix above.
## If the inverse has already been calculated and the matrix has not changed,
## then cacheSolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}

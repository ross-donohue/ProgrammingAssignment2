## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix saves the matrix provided in the environment for a list of functions
## these function provide options to a) replace the matrix, b) retrive the matrix, 
## c) establish an inverse matrix, or d) retrieve the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
 I <- NULL
 set <- function(y){
         x <<- y
         I <<- NULL
 }
 get <- function() x
 setinverse <- function(inverse) I <<- inverse
 getinverse <- function() I
 list(set = set, get = get, 
      setinverse = setinverse,
      getinverse = getinverse)
}


## cacheSolve checks to see if the Inverse has already been calculated 
## since the last change and either: retrieves the cached inverse matrix, or
## calculates the new inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        I <- x$getinverse()
        if(!is.null(I)){
                message("retrieving cached inverse matrix")
                return(I)
        }
        matrix <- x$get()
        I <- solve(matrix)
        x$setinverse(I)
        I
}

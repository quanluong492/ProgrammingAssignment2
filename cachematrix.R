# get example of matrix
x <- matrix(c(2,0,0,0,2,0,0,0,2), nrow = 3, ncol = 3, byrow = T)
## Put comments here that give an overall description of what your
## functions do
  
## Write a short comment describing this function
  
# Create a matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    # cached inverse of matrix
    inv <- NULL
    
    ## getter/setter for matrix
    get <- function() x
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    
    ## getter/setter for matrix inverse
    getinv <- function() inv
    setinv <- function(inverse) inv <<- inverse
    
    ## return list of functions for matrix
    list(get=get, set=set, getinv=getinv, setinv=setinv)
    
}
  
# function to cache the inversed matrix
  
## Write a short comment describing this function
  
cacheSolve <- function(x, ...) {
   inv <- x$getinv()
    
    # if the inversed matrix is already computed, it will show the cached inverse matrix
    if (!is.null(inv)) {
      message("inverse is cached")
      return(inv)
    }
    
    # if inversed matrix is not computed, function will compute inverse of matrix 
    m <- x$get()
    inv <- solve(m, ...)
    
    # cache inversed matrix
    x$setinv(inv)
    
    # return inversed matrix
    return(inv)
    ## Return a matrix that is the inverse of 'x'
}
  
x2 <- makeCacheMatrix(x)
cacheSolve(x2)
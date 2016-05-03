## Assignment: Caching the Inverse of a Matrix
 


## makeCacheMatrix is a function closure, containing 
## inner functions with different outcome.
## It is going to be four of them.

makeCacheMatrix <- function(x = matrix()) {
      ## setting m to zero
      m <- NULL 
      ## setmat sets the matrix
      setmat <- function(y) {
      x <<- y
      m <<- NULL
      }
  
      ## getmat gets the matrix
      getmat <- function() x
  
      ## setinv inverses the matrix w/ inner function solve()
      ## and caches it
      setinv <- function(solve) m <<- inv
  
      ## getinv gets the value of m, which is inverted matrix
      getinv <- function() m
      ## this is the function output,
      ## vector containing all inner functions
      list(setmat = setmat, getmat = getmat,
        setinv = setinv,
        getinv = getinv)
      }




## cachesolve checks if the inverse of matrix exists,
## otherwise reverses matrix.
cacheSolve <- function(x, ...) {
  
      ## using subset of makeCachematrix function 
      ## to use its inner function getinv()
      ## to get cached m value
      m <- x$getinv()
  
      ## checking if cached m exists
      if(!is.null(m)) {
    
        ## returns the inverse of matrix with comment
        message("getting cached data")
        return(m)
      }
  
      ## using subset of makeCachematrix function 
      ## to use its inner function getmat()
      ## to get matrix  
      data <- x$getmat()
      
      ## reversing the matrix...
      m <- solve(data, ...)
      
      ## using subset of makeCachematrix function 
      ## to use its inner function getmat()
      ## to inverse matrix and to cache it
      x$setinv(m)
      
      ## function product, inverted matrix
      m
}


        #### Thank you for reading my code :)


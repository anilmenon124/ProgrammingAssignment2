## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(m = matrix()) {

inverse <- NULL # inverse starts uncomputed
   
    ## set(matrix)
    ## sets the underlying matrix and invalidates the cached inverse
    set <- function(y) 
    {
         m <<- y
         inverse <<- NULL
     }

 
  ## get()
   ## return the underlying matrix
   get <- function()
    {
        m
    }
    
   ## setinverse(matrix)
    ## sets (but does not compute!) the value of the inverse of the underlying
    ## matrix
    setinverse <- function(inv) 
    {
        inverse <<- inv
    }
    
    ## return the cached value of the inverse of the matrix.
    ## returns NULL if the inverse has not yet be set, or the underlying
    ##   matrix has changed since the last invocation
    getinverse <- function() 
    {
      inverse
    }
 
   ## return a list of getter and setter methods as a result of object creation
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)


}


## Write a short comment describing this function

cacheSolve <- function(m, ...) {
        ## Return a matrix that is the inverse of 'm'
        
        ## Return a matrix that is the inverse of 'm'

 ## Return a matrix that is the inverse of 'm'
     inv <- m$getinverse()

   if(!is.null(inv)) 
   {
       ## Inverse already calculated, value not stale
         message("getting cached data")
         return(inv)
     }
   
   ## Either inverse has never been calculated or cached inverse is stale
     data <- m$get()
     inv <- solve(data, ...)
     m$setinverse(inv)
     inv

}

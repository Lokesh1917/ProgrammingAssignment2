## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ##This function creates a special "matrix" object that can cache its inverse.
        inv <- NULL  ## intialising a null matrix to store inverse of a matrix
        set <- function(y){         ##defining set function
         x <<- y                   ##passing value of y to parent x    
        inv <<- NULL             
        }
        get <- function() x  ## defining get function
        setInverse <- function(inverse) inv <<- inverse ##defining setInverse function
        getInverse <- function() inv ##defining getInverse function
        list(set=set,get=get,setInverse=setInverse,getInverse=getInverse) ##creating a list 
}


## Write a short comment describing this function
##function calculates the inverse of the special "matrix" created with the above function
##If the inverse has already been calculated (and the matrix has not changed)
##The cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
       inv<- x$getInverse()    
       if(!is.null(inv)) {         
       message("getting cached data")    
       return(inv)           
       }
       data <- x$get()          
       inv<- solve(data, ...)   
       x$setInverse(inv)
       inv
}
        
}

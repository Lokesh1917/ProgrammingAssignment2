## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ##This function creates a special "matrix" object that can cache its inverse.
        inv <- NULL  
        set <- function(y){         
         x <<- y                       
        inv <<- NULL             
        }
        get <- function() x  
        setInverse <- function(inverse) inv <<- inverse 
        getInverse <- function() inv 
        list(set=set,get=get,setInverse=setInverse,getInverse=getInverse) 
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

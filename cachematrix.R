## Below are two functions that can be used to create a special matrix object and 
## cache's its inverse.

## The first function, makeCacheMatrix creates a special "matrix" object, 
## with special it's special ability to:
## 1)set the value of the matrix 
## 2)get the value of the matrix
## 3)set the value of the inverse 
## 4)get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL                     
    set <- function(y) {         
        x <<- y                     
        i <<- NULL
    }
    get <- function() x              
    setInverse <- function(inverse) i <<- inverse    
    getInverse <- function() i
    list(set = set, set = set,
         setInverse = setInverse,
         getInverse = getInverse)
}


## The following function calculates the inverse of the special "matrix" created with the above function.
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the 'setInverse' function.

cacheSolve <- function(x, ...) {
    i <- x$getInverse()
        if(!is.null(i)) {
            message("getting cached data")
            return(i)
        }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}

## These R file contains functions to:
## 1 - Create a special kind of matrix that caches its inverse matrix;
## 2 - Calculate the inverse matrix of this special matrix.


## Create a special matrix that caches its inverse matrix since it is calculated.
## Receives as argument a normal matrix or a empty matrix by default.
## Returns a list of functions to manipulate the matrix and the cached inverse matrix. 
## These functions are described below:
## set: Sets a new matrix and clear the cache
## get: Gets the original matrix
## setinverse: Sets the inverse matrix to cache
## getinverse: Gets the inverse matrix from cache

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function(inverse) inv <<- inverse
    
    getinverse <- function() inv
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Calculates the inverse matrix of the special "matrix" created with the above function.
## However, it first checks to see if the inverse matrix has already been calculated. 
## If so, it gets the inverse matrix from the cache and skips the computation. 
## Otherwise, it calculates the inverse matrix of the data and sets the value of the 
## inverse matrix in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if (!is.null(inv)) {
        message("getting inverse from cache")
        return(inv)
    }
    
    m <- x$get()
    inv <- solve(m)
    x$setinverse(inv)
    
    inv
}

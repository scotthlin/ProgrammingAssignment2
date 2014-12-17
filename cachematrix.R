## This R script aims at caching results of inversing a matrix to speed up the process.
## It's split up into two functions, one which caches the results and one to inverse the matrix. 

## makeCacheMatrix is a function that takes a required square matrix and creates a list object
## that includes functions to reset the matrix, get the original matrix, and cache the inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL                                   #object meant to hold inverse values
    set <- function(y = matrix()) {             #reset the cache and matrix to new dimensions. 
        x <<- y
        m <<- NULL
    }
    get <- function() x                         #Gets original matrix
    setinverse <- function(inv) m <<- inv       #Function used to cache results from cacheSolve
    getinverse <- function() m                  #Function used to return the inverse
    list(set = set, get = get,                  #Creation of the list object for the matrix
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve is a function that takes a makeCacheMatrix object and checks to see if the inverse
## is cached. If not, it'll invert it and both return the results and set the result to cache.

cacheSolve <- function(x, ...) {
    z <- x$getinverse()                         #checks if an inverse exists
    if(!is.null(z)) {                           #if inverse is in cache, it'll return that value
        message("getting cached data")
        return(z)
    }
    
    x$setinverse(solve(x$get()))                #if not, it'll calculate the inverse and set it to the x object
    return(x$getinverse())                      #and return the x object
}

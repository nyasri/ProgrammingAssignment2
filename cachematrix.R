## Function to Cache the inverse of a matrix
## functions do

## makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of matrix
## get the value of the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function(inverse) i <<- inverse
    
    getinverse <- function() i
    
    list(set=set, get=get,
         setinverse=setinverse, getinverse=getinverse)

}


## The cacheSolve function calculates the inverse of the special "matrix" created by makeCacheMatrix function
## It checks to see if the inverse of the matrix is already calculated
## If 'yes', it gets the inverse from the cache and avoids computation
## If 'no', it calculates the inverse and sets the value of inverse in cache using setinverse function

cacheSolve <- function(x, ...) {
    
        ## Return a matrix that is the inverse of 'x'
        
        i <- x$getinverse()
        
        if(!is.null(i)) {
            message("getting cached data")
            return(i)
        }
        
        data <- x$get()
        i <- solve(data,...)
        x$setinverse(i)
        i
}

## Test output steps -
## source('cachematrix.R')
## m <-makeCacheMatrix(matrix(c(2, 3, 3, 2), c(2, 2)))
## cacheSolve(m) ***calculates inverse and sets value
## cacheSolve(m) ***to see the message 'getting cached data'

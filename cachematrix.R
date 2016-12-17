## Put comments here that give an overall description of what your
## functions do
## Caching Inverse of a Matrix


## Write a short comment describing this function
## makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        # Changes when the user sets the value
        inv <- NULL
        # Sets  matrix not the inverse
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        ##gets the matrix not the invers
        get <- function() x
        ##sets the invers
        setinverse <- function(inverse) inv <<- inverse
        ##get the invers
        getinverse <- function() inv
        ##returns list of functions for matrix
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
        
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ##Sees if it has been computed, if not, returns cached matrix inverse
        inv <- x$getinverse()
        ##if not computed return cached matrix invers
        if(!is.null(inv)) {
                message("getting cached data.")
                return(inv)
        }
        
        ##compute matrix inverse
        data <- x$get()
        inv <- solve(data)
        ##cache invers
        x$setinverse(inv)
        ##return the inverse
        inv
}

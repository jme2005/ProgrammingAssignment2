## These functions creates a special matrix object and computes and cache its inverse
## to prevent unnessary costly computations. This function assumes that the matrix
## provided is inversible and will return an error if it's not.


## Creates a special matrix object that can cache it's inverse.

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        get <-function() x      #function returns the matrix x stored in the local environment
        setinv <- function(inv) m<<-inv #function set the value of m to the inverse of the matrix to the parent environment
        getinv <- function() m          #returns the value of the inverse
        list(get=get,
             setinv=setinv,
             getinv=getinv)     # returns the matrix object that's
}


## Takes the matrix object created with the makeCacheMatrix function and returns the inverse 
## of the matrix by retrieving from cache if it's been calculated
## or return calculate it and store it in cache if it has not been previously calculated.

cacheSolve <- function(x, ...) {
        inv<-x$getinv()         # gets the inverse from the matrix object
        if(!is.null(inv)){      # if the inverse of the matrix is available the inverse is returned
                message("getting cached data")
                return(inv)     # return the inverse of the matrix
        }
        data<-x$get()           # if it's not available get the matrix
        inv<-solve(data,...)    # calculate the inverse
        x$setinv(inv)           # store the inverse in cache of the local environment of makeCacheMatrix
        inv                     # Return the inverse of the matrix
        
}

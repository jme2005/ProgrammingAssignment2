## These functions creates a special matrix object and computes and cache its inverse
## to prevent unnessary costly computations. This function assumes that the matrix
## provided is inversible and will return an error if it's not.


## Creates a special matrix object that can cache it's inverse.

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set <-function(y) {
                x<<-y
                m<<-NULL
        }
        get <-function() x
        setinv <- function(inv) m<<-inv
        getinv <- function() m
        list(set=set,get=get,
             setinv=setinv,
             getinv=getinv)
}


## Returns the inverse of a matrix by retrieving from cache if it's been calculated
## or return calculate it and store it in cache if it has not been previously calculated.

cacheSolve <- function(x, ...) {
        inv<-x$getinv()         # gets the inverse from the matrix object
        if(!is.null(inv)){      # if the inverse of the matrix is available the inverse is returned
                message("getting cached data")
                return(inv)     # return the inverse of the matrix
        }
        data<-x$get()           # if it's not available get the matrix
        inv<-solve(data,...)    # calculate the inverse
        x$setinv(inv)           # store the inverse in cache
        inv                     # Return the inverse of the matrix
        
}

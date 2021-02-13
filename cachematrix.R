## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL            # Setting inv to null
        
        set <- function(y){                # Creating set function for input of matrix
                x <<- y                    # Storing matrix will "<<-" operator so result can be stored globally
                inv <<- NULL               # Setting inv to null as we need have assigned new matrix and previous inv value should be set to NULL 
        }
        get <- function() {x}           
        setInverse <- function(inverse) {inv <<- inverse}
        getInverse <- function() {inv}            # gets the inverse of the matrix
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
        
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()        # Checks for the inverse of matrix
        if(!is.null(inv)){
                message("getting cached data")      # If already calculated then it will fetch it from cache
                return(inv)
        }
        mat <- x$get()       # If its not already calculated then we will fing the inverse of matrix
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
        
}

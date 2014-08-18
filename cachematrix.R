## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##Cache the inverted matrix. This function if 
##called used in cacheSolve function
makeCacheMatrix <- function(x = matrix()) {
        ##inverted matrix
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        ##return the matrix
        get <- function() x
        
        ##Set the inverted matrix in s
        setsolve <- function(solve) s <<- solve
        
        ##Get the inverted matrix s
        getsolve <- function() s
        
        ##List representation of cached inverted matrix
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}


## Write a short comment describing this function
##This function only calculate the matrix 
##invertion if the was not calculate yet.
cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        ##On second running output getting cached data
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        
        #Get the matrix that is stored in makeCacheMatrix x
        data <- x$get()
        ##Calculate the inversion matrix
        s <- solve(data, ...)
        ##Store the inversion matrix in x
        x$setsolve(s)
        ##return invertion
        s
}


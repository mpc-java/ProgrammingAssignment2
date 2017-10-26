## Function makeCacheMatrix takes a matrix as input argument, set and get the values of that matrix and it's inverse matrix.

## Function to make a cacheMatrix

makeCacheMatrix <- function(x = matrix()) {
        inverseMatrix <- NULL
        
        set <- function(y) {
                x <<- y
                inverseMatrix <<- NULL
        }
        
        get <- function() x
        
        setInverse <- function(inverse) inverseMatrix <<- inverse
        
        getInverse <- function() inverseMatrix
        
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Function that cache the inverse of a matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverseMatrix <- x$getInverse()
        
        if(!is.null(inverseMatrix)) { 
                message("getting cached inverse matrix")
                return(inverseMatrix)
        }
        
        matrix <- x$get() 
        # Used solve function to compute inverse of a matrix
        inverseMatrix <- solve(matrix, ...)
        x$setInverse(inverseMatrix)
        inverseMatrix
}

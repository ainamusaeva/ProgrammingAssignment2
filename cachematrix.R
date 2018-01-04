## makeCacheMatrix and cacheSolve functions compute and save a matrix inverse
## and check if a matrix inverse is computed already to avoid the need to
## compute it again

## makeCacheMatrix takes an invertible matrix as an input
## then calculates its inverse and saves it for later use

makeCacheMatrix <- function(x = matrix()) {
        # mInv is the variable for a matrix inverse 
        mInv <- NULL
        set <- function(y) {
                x <<- y
                mInv <<- NULL
        }
        get <- function() x
        # The solve() function calculates the inverse of a matrix
        setinverse <- function() mInv <<- solve(x)
        getinverse <- function() mInv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve checks if a matrix's inverse has been computed
## if so, it just returns the inverse
## otherwise, it computes a matrix inverse and saves it for later use

cacheSolve <- function(x, ...) {
		# Get matrix inverse
        mInv <- x$getinverse()
        # If it is not null, don't worry about calculating it again and just return the value
        if(!is.null(mInv)) {
                message("matrix inverse has been calculated already, getting the cached matrix")
                return(mInv)
        }
        matr <- x$get()
        # Calculate matrix inverse and save it to be able to get the cached value later
        mInv <- solve(matr, ...)
        x$setinverse(mInv)
        mInv
}

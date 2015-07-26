## Below is a pair of functions that cache the inverse of a matrix
## 

## This function creates a special "matrix" object that can cache its inverse.
## The makeCacheMatrix function stores a list of functions to store and retrieve the matrix
## and then to set and get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        invmatrix <- NULL
        set <- function(y) {
                x <<- y
                invmatrix <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) invmatrix <<- inv
        getinverse <- function() invmatrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invmatrix <- x$getinverse()
        if(!is.null(invmatrix)) { # if inverse data already exists, it is just retrieved from cache
                message("getting cached data")
                return(invmatrix)
        }
        data <- x$get() # if inverse data doesn't exist use get function in makeCacheMatrix to get the matrix data
        invmatrix <- solve(data, ...)  # then compute the inverse with solve() function
        x$setinverse(invmatrix) # then use setinverse function from makeCacheMatrix to cache for later
        invmatrix
}

## Function makeCacheMatrix(x)
## Returns a list containing 4 functions:
## set : sets the value of the matrix x
## get : gets the value of the matrix x
## setinv : sets the value of the inverse matrix
## getinv : gets the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## Function cacheSolve(x, ...)
## Returns the inverse of the special matrix x created
## with makeCacheMatrix. If the inverse has already been
## calculated, it is retrieved from the cache, otherwise
## it is calculated and stored in the cache
cacheSolve <- function(x, ...) {
        ## check whether the inverse is already in the cache
        inverse <- x$getinv()
        if (! is.null(inverse)) {
                ## inverse in the cache, no calculation required
                message("getting cached inverse matrix")
                return(inverse)
        }
        
        ## inverse not in the cache, calculation required
        ## get the matrix
        data <- x$get()
        
        ## calculate the inverse
        inverse <- solve(data, ...)
        
        ## store the inverse in the cache
        x$setinv(inverse)
        
        ## return the inverse
        inverse
}

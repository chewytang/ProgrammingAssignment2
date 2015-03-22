## The functions use scoping rules to store the
## inverse of a given matrix in cache, and enables
## checking of the cache when the inverse function
## is called.  If the cache contains cached information
## the function returns the cached matrix without
## running the inverse function.

## This function creates a list containing a function to
## set the value of the matrix, get the value of the matrix,
## set the value of the inverse, and get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL       
        set <- function(y) {    ## Sets matrix value in cache
                x <<- y         ## Stores matrix in cache
                m <<- NULL      ## Resets m to null
        }
        get <- function() x     ## Gets matrix value
        setinverse <- solve(inverse) m <<- inverse  ## Sets inverse in cache
        getinverse <- solve() m         ## Gets inverse
        list(set = set, get = get,      ## Creates list of function values
                setinverse = setinverse,
                getmean = getinverse)
}


## The following function calculates the mean of the special
## "vector" created with the above function. However, it first
## checks to see if the mean has already been calculated. If so,
## it gets the mean from the cache and skips the computation. 
## Otherwise, it calculates the mean of the data and sets the 
## value of the mean in the cache via the setmean function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()     ## Defines m by calling cache
        if(!is.null(m)) {       ## Checks if m is null
                message("getting cached data")
                return(m)       ## Ends function if m is not null
        }
        data <- x$get()         ## Pulls matrix data
        m <- solve(data, ...)   ## Returns inverse of matrix
        x$setinverse(m)         ## Sets inverse in cache
        m                       ## Prints inverse
}

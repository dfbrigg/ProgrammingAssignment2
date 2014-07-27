#Programming Assignment 2 - Caching the Inverse of a Matrix
#Write an R function to cache time-consuming computations
#So, we need two functions as follows:
#       Function 1 
#       makeCacheMatrix: This function creates a special "matrix" object that can 
#       cache its inverse.
#       So we create a class called makeMatrix, this class has behaviors 
#       (methods) that can be called once your object has been created.
#       makeMatrix starts by:
#       -initialise the matrix inverse cache value
#       -'set'assigns variables x & m to be available in the global environment and callable
#       -'get' when called, returns the stored value of x
#       -'setinv' when called, caches the inverse of the matrix
#       -'getinv' when called, returns the previously cached value
#
makeMatrix <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


#       Function 2
#       cacheSolve: This function computes the inverse of the special "matrix" 
#       returned by makeMatrix above. 
#       If the inverse has already been calculated (and the matrix has not 
#       changed), then cacheSolve should retrieve the inverse from the cache.
#       -'m'is assigned the value of previous inverse calculation by calling
#       the 'getinv'method of makeMatrix.
#       -the 'íf'statement checks whether 'm' stores a value and has not just 
#       been initialised. If there is a value, the simply assumption is the 
#       matrix has not changed and return the cached value.
#       - if this is first time the inverse for the matrix has been calculated,
#       then use the 'solve' function to calculate the inverse, return the 
#       result as 'm' and cache the result by calling the 'setínv' method of 
#       makeMatrix.
#       value 
cacheInv <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("I answered this question earlier ;)")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinv(m)
        m
}
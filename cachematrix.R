# Special Matrix which exposes to 4 functions to enable caching of a matrix.
#
# Args:
#   x: Matrix for which its inverse cache needs to be maintained. 
#
# Returns:
#   List of Utility Functions
#       get :- To get the original Matrix
#       set :- To set a new Matrix .
#       getInverse :- To get the cached inverse of the matrix.
#       setInverse :- To set(cache) the inverse the matrix.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() {
                x                
        }
        setInverse <- function(matrix) {
                m <<- matrix
        }
        getInverse <- function() {
                m
        }
        # Returns List of usable functions
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


# Function to get Inverse of a Special Cached Matrix(matrix created by using makeCacheMatrix).
# In the First call to get the inverse of a matrix, it computes the Inverse of that matrix and 
# stores it. Then all subsequent calls to get the inverse of that same matrix, it returns the 
# same cached value.
#
# Args:
#   x: Special Cache Matrix whose inverse needs to calculated. Cache Matrix is created by 
#       calling "makeCacheMatrix" function.
#   ... : further arguments passed to "solve" R method.
#
# Returns:
#   Inverse of the given Matrix.
#
# Sample Usage: 
#   x <- matrix(1:4, 2 ,2)
#   cacheX = makeCacheMatrix(x)
#   inverse = cacheSolve(cacheX)

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        #  If we get inverse on special cache matrix as not null 
        #  ,we return the cached Value.
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        # Otherwise we get the matrix, calculate the inverse using "solve" R function.
        # Then cache this calculated inverse using "setInverse" and 
        # return this newly cached value.
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
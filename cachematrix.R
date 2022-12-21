## Catching the Inverse of a Matrix
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## This function is to create a Matrix that can catch the Inverse
## Firstly, I set x as a matrix with values as below, set My_inv is NULL, then solve this

makeCacheMatrix <- function(x = matrix(1:6,3,3)) {
        my_inv <- NULL
        set <- function(y) {
                x <<- y
                my_inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) my_inv <<- inverse
        getInverse <- function() my_inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse
        ) 
}


## This function created by makeCacheMatrix above. 
## If the inverse has already been calculated, it will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        my_inv <- x$getInverse()
        if (!is.null(my_inv)) {
                message("get inversed matrix")
                return(my_inv)
        }
        num <- x$get()
        my_inv <- solve(num, ...)
        x$setInverse(my_inv)
        my_inv
}

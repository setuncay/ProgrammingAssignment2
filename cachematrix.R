## This file exposes two functions to provide cached matrix inversion:
## makeCacheMatrix: 
## This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: 
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

## creates a 'cached' matrix which actually is a list of functions to:
## get the underlying matrix.
## set the underlying matrix.
## gets the inverse of the matrix.
## set the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        ## Holds inverse of the matrix.
        matrix_inverse <- NULL
        ## Sets the matrix.
        set <- function(y) {
          x <<- y
          matrix_inverse <<- NULL
        }
        ## Gets the matrix
        get <- function() x
        ## Sets the Matrix Inverse
        setInverse <- function(inverse) matrix_inverse <<- inverse
        ## Gets the Matrix Inverse
        getInverse <- function() matrix_inverse
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## get the inverse (if exists)
        existing_inverse <- x$getInverse()
        if (!is.null(existing_inverse)) {
          ## Matrix inverse is already cached, return cached value.
          message('getting inverse of the matrix from cache')
          return (existing_inverse)
        } else {
          matrix_data <- x$get()
          inverse_matrix_data <- solve(matrix_data, ...)
          x$setInverse(inverse_matrix_data)
          inverse_matrix_data
        }
}

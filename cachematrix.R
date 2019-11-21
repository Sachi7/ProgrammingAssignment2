## makeCacheMatrix ns cacheSolve are functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
     inv_matrix <- NULL
     set <- function(y) {
           x <<- y
           inv_matrix <<- NULL
     }
     get <- function() x
     setinverse <- function(solve) inv_matrix <<- solve
     getinverse <- function() inv_matrix
     list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
     inv_matrix <- x$getinverse()
     if (!is.null(inv_matrix)) {
           message("getting cached data")
           return(inv_matrix)
     }
     data <- x$get()
     inv_matrix <- solve(data)
     x$setinverse(inv_matrix)
     inv_matrix
}

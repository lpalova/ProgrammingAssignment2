### Put comments here that give an overall description of what your
### functions do
## The following functions cache the inverse of a matrix.

### Write a short comment describing this function
## makeCacheMatrix: This function creates a special "matrix" object 
## (CacheMatrix) that can cache its inverse 'inv'.
##
## For example, a CacheMatrix 'my_matrix' is created by calling:
## my_matrix <- makeCacheMatrix()
## 
## makeCacheMatrix returns a list of functions:
## (i)   function 'set()' sets the value/s of the underlying R matrix; 
##       e.g. my_matrix$set(matrix(1:4,2,2))
##
## (ii)  function 'get()' returns the underlying R matrix; 
##       e.g. my_matrix$get()
## 
## (iii) function 'setinv(inverse)' stores the inverse of the underlying 
##       R matrix inside the CacheMatrix
##
##  (iv) function 'getinv()' returns the cached inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


### Write a short comment describing this function
## cacheSolve: This function operates on CacheMatrix objects. It computes 
## the inverse of the CacheMatrix. If the inverse has already been computed
## (and the matrix has not changed), cached value is returned. 
## Otherwise, the function calls get() function on the CacheMatrix to retrieve
## the corresponding R matrix, computes the inverse, stores the inverse in 
## the CacheMatrix and returns the inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}

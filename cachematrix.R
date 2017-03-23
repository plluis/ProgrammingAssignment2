## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function: The function will create a special matrix than can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
            inv <- NULL
            set <- function(y) {
                  x <<- y
                  i <<- NULL
            }
            get <- function() x
            setinv <- function(inverse) inv <<- inverse
            getinv <- function() inv
            list(set = set, get = get,
               setinv = setinv,
               getinv = getinv)
}


## Write a short comment describing this function: The Function will compute the inverse of the matrix that makeCacheMatrix will return.

cacheSolve <- function(x, ...) {   
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

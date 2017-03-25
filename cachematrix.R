## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function: The function will create a special matrix than can cache its inverse

makeCacheMatrix <- function(x = matrix()) { 
            inv <- NULL   ##here the function sets a new variable called inv
            set <- function(y) {  ##set is a new function
                  x <<- y  ##the <<- operator works in a different environment
                  inv <<- NULL
            }
            get <- function() x
            setinv <- function(inverse) inv <<- inverse
            getinv <- function() inv
            list(set = set, get = get, ##the function will get the value of the matrix and the inverse
               setinv = setinv,
               getinv = getinv)
}


## Write a short comment describing this function: The Function will compute the inverse of the matrix that makeCacheMatrix will return.

cacheSolve <- function(x, ...) {   
         inv <- x$getinv()   ##in this line of code I ask for the getinv of x
         if(!is.null(inv)) {
                message("getting cached data") ##Here the function will check if the previous function works
                return(inv)
         }
         data <- x$get()
         inv <- solve(data, ...) ## with solve() it will return the inverse that I need
         x$setinv(inv) 
         inv
}

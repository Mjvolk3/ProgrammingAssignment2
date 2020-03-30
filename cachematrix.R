## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function returns a special lists containing functions to:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the mean
# 4. get the value of the mean
makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Write a short comment describing this function
#The following function calcuates the inverse of the special "matrix" first checking to see if the 
#the inverse has been cached. If it has, the value is inverse is returned, if not, the inverse is 
#calculated and cached. Due to the set function, the cacheSolve function will catch when speical 
#matrix has been changed or updated and the inverse will be updated and cached accordingly.
cacheSolve <- function(x, ...) {
        
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}






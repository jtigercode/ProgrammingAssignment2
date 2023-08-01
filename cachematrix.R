## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL                #intializing inverse as the NULL
        set <- function(y){ 
                x <<- y
                inv <<- NULL
                }
        get <- function()x               #function to get the matrix x
        setinv <- function(inverse) inv <<- inverse 
        getinv <- function(){
                inver <- ginv(x)
                inver%*%x                        #function to get the inverse of the matrix
                }
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {                #obtains the cache data
        inv <- x$getinv()
        if(!is.null(inv)){                        #checking for NULL inverse        
                message("getting cached data")
                return(inv)                        #returns the inverse as NULL
       
}
data <- x$get()
inv <- solve(data, ...)                #inverse calculation
x$setinv(inv)
inv                 ## Return a matrix that is the inverse of 'x'
}

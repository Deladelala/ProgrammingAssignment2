## Put comments here that give an overall description of what your
## functions do
## This pair of functions are used to cache the inverse of a matrix.

## Write a short comment describing this function
## The "makeCacheMatrix" function creats a special "matrix" that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function(){x}
        setinverse <- function(inverse){i <<-inverse}
        getinverse <- function(){i}
        
        list( set= set, get= get, 
              setinverse= setinverse, getinverse=getinverse)
}


## Write a short comment describing this function
## The "cacheSolve" function computes the inverse of the "matrix" returned by the above function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data,...)
        x$setinverse(i)
        i
}

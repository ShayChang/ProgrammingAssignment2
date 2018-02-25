## These two functions work as the short-cut for time-consuming matrix inversion computation.
## `makeCacheMatrix` function creates a special "matrix" object that can cache its inverse.
## `makeCacheMatrix` creates a special "matrix", which is a list containing a function to
##1.  set the value of the matrix
##2.  get the value of the matrix
##3.  set the value of the inverse of original matirx
##4.  get the value of the inverse of original matirx



makeCacheMatrix <- function(x = matrix()) {
    i<- NULL
    set<- function(y) {
        x<<- y
        i<<- NULL
    }
    get <- function() x
    setinverse <- function(inverse)  i<<- inverse
    getinverse<- function() i
    list(set=set, get=get,
    setinverse=setinverse,
    getinverse=getinverse)
    
}


## `cacheSolve`function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above.
## First, it checks that whether the inverse has already been calculated before (and the matrix has not changed).
## If so, it has been cached, then `cacheSolve` should retrieve the inverse from the cache.
## Otherwise, `cacheSolve` function will compute the inverse of matrix, and return its inversed value in the cache via the `setinverse` function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i<-x$getinverse()
    if(!is.null(i)) {
        message("getting cached matrix")
        return(i)
    }
    matrix<- x$get()
    i<- inverse(matrix, ...)
    x$setinverse(i)
    i
}


## This function creates a "matrix" inverse it and creates cache for further computation by the next function

makeCacheMatrix <- function(x = matrix()) {
invr <- NULL  
set <- function(func) {
x <<- func
invr <<- NULL 
}
get <- function() x 
setinverse <- function(inverse) invr <<- inverse
getinverse <- function() invr
list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  
}


## This function computes inverse of the matrix retruned by above function by using the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
invr <- x$getinverse()
if(!is.null(invr)) {
return(invr)
}
data <- x$get()
invr <- solve(data, ...)
x$setinverse(invr)
invr
}

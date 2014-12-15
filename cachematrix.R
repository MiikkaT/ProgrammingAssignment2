## The following pair of functions (makeCacheMatrix and cacheSolve) cache the inverse
## of a matrix. The code is adapted, with only minor modifications,
## from the example given in course web page.When using these functions, use the object
## retuned by makeCacheMatrix as an argument in cacheSolve function. 

## The makeCacheMatrix function creates a special "matrix" object that can
## cache its inverse. As its argument, the functions takes a matrix object. The resulting
## "matrix" object is actually a list of functions. With these functions you can
## set a new matrix 'x', get the matrix 'x', set (but not calculate) the inverse of the
## matrix 'x' and get the inverse of the matrix 'x'.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse_mat) inv <<- inverse_mat
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## The cacheSolve function returns a matrix that is inverse of the matrix 'x' defined by
## the makeCacheMatrix function.If the inverse has already been calculated, the cachesolve
## should retrieve the inverse from the cache. Otherwise, it calculates the inverse of
## the matrix and sets the inverse matrix in the cache via the setinverse function

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached matrix")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
        
}

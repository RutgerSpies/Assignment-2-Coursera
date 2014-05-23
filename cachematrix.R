## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
## makeCacheMatrix reates a special "matrix", which is really a list containing a function to

    ## set the value of the matrix
    ## get the value of the matrix
    ## set the inverse of the matrix
    ## get the inverse of the matrix


makeCacheMatrix <- function(a = matrix()) {
    inv<- NULL
    set <- function(y) {
    a <<- b
    inv <<- NULL
    }

        get <- function() a
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## The cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(a, ...) {
        ## Return a matrix that is the inverse of 'a'
    inv <- a$getinverse()
    if(!is.null(inv)) {
    message("reading cache...")
    return(inv)
    }
        data <- a$get()
        inv <- solve(data)
        a$setinverse(inv)
        inv
}

## Test
## a = matrix (1:100, 2, 2)
## m = makeCacheMatrix (a)
## m$get()
##            [,1] [,2]
##       [1,]    1    3
##       [2,]    2    4
## cacheSolve(m)
##            [,1] [,2]
##      [1,]   -2  1.5
##      [2,]    1 -0.5
##
## cacheSolve(m)
## reading cache...
##            [,1] [,2]
##      [1,]   -2  1.5
##      [2,]    1 -0.5


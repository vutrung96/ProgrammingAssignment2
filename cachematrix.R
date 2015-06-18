##This function takes a matrix as an input and returns its inverse 
##as the output. Most importantly, it caches the inverse of the matrix
##so that, if it has to calculate the inverse of the same matrix again,
##it can return the cached value without having to calculate the inverse.

## The makeCacheMatrix function takes a matrix as an input and creates
## a list out of that matrix so that its inverse can be cached.
## The list includes four functions:
## 1. set(): set the value of the matrix
## 2. get(): get the value of the matrix
## 3. setinverse(): set the inverse of the matrix
## 4. getinverse(): get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL 
        set <- function(y) { 
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) inverse <<- inv
        getinverse <- function() inverse
        list(set = set, get = get, setinverse = setinverse, 
             getinverse = getinverse)
}


## The cacheSolve function takes a matrix created by the makeCacheMatrix function
## and returns its inverse (The matrix is actually a list of four functions).
## If the inverse of the matrix has already been cached, it returns the cached 
## value. If the inverse of the matrix has not been cached, it calculates the
## inverse, cached the inverse, and returns the calculated inverse.

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse() 
        if (!is.null(inverse)) {
                print("getting result from cache")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data)
        x$setinverse(inverse)
        inverse
}

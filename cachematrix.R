## The following two functions are used to cache the inverse of a matrix. 
## Caching the inverse of a matrix saves us potentially time-consuming computations.
## Therefore, it makes sense to cache the inverse of a matrix instead of computing it
## repeatedly. 

## The first function, 'makeCacheMatrix' creates a list containing a function to

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix



makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {                    # set the value of the matrix
        x <<- y
        inv <<- NULL
    }
    get <- function() {                     # get the value of the matrix
        x
    }
    setinverse <- function(inverse) {       # set the value of the inverse
        inv <<- inverse
    }
    getinverse <- function() {              # get the value of the inverse
        inv
    }
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) # Return list of functions
}



## The next function returns a matrix that is the inverse of 'x'
## created with the above function. However, it first checks to see if the 
## inverse has already been calculated. If so, it get's the inverse from the
## cache and skips the computerization. Otherwise, it calculates the inverse of 
## the data and sets the value of the inverse in the cache via the 'setinverse'
## function. 

## For this assignment, we assume that the matrix supplied is always invertible

cacheSolve <- function (x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {                     # if the inverse was cached - 
        message("getting cached data.")
        return(inv)                         # exit program without executing subsequent code
    }
    data <- x$get()                         # otherwise, put the data in 'data'
    inv <- solve(data, ...)                 # compute the inverse of the data
    x$setinverse(inv)                       # call function to cache the inverse
    inv                                     # return the inverse
}
## This function is used to cache the inverse of a matrix. The first function creates a
## matrix that can cache its inverse. The second function checks if the inverse already exists 
## and if the inverse does not exist, computes the inverse.

## This function creates a special "matrix" object that can cache its inverse. The function
## creates a list containing functions to 
## 1.set the value of the matrix - setmatrix
## 2.get the value of the matrix - getmatrix
## 3.set the value of the inverse of the matrix - setinverse
## 4.get the value of the inverse of the matrix - getinverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        setmatrix <- function(y) {
                x <<- y
                i <<- NULL
        }
        getmatrix <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(setmatrix = setmatrix, getmatrix = getmatrix,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                #message("getting cached data")
                return(i)
        }
        data <- x$getmatrix()
        i <- solve(data)
        x$setinverse(i)
        i
}

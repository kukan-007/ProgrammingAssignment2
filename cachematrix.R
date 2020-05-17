## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL                             ## initailize inv to NULL which holds Inverse of a Matrix passed
    set <- function(y) {                    ## define the set function to assign new 
        x <<- y                             ## value of matrix in parent environment
        inv <<- NULL                        ## if already new matrix is present, Reset to NULL
    }
        
    get <- function() x                     ## define the get fucntion that returns the value of the matrix arguement

    setinverse <- function(inverse) inv <<- inverse  ## assigns value for invin parent environment 
    getinverse <- function() inv                     ## gets value when called 
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  ## you need this in order to refer 
                                                                                  ## to the functions with the $ operator

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {                         # Check inverse matrix is NULL or not
        message("getting cached data")
        return(inv)                             # Gets invertible Matrix
    }
    data <- x$get()                             # gets original Data Matrix
    inv <- solve(data, ...)                     # Solving the Matrix to get inverse
    x$setinverse(inv)
    inv                                         # returns invertible matrix
}

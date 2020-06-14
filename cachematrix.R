## MakeCacheMatrix, in this we assume that th =e matrix is invertible
## Then we will set the value of matrix
## Then we will set the inverse of the matrix
## Then we create a list

## This function creates a special matrix 

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
           x <<- y
           inv <<- NULL
    }
    get <- function() {x}
    SetInverse <- function(inverse) {inv <<- inverse}
    getInverse <- function() {inv}
    list(set = set, get = get, SetInverse = SetInverse, getInverse = getInverse)
}


## This function computes inverse of the matrix with the above mentioned function

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)){
          message("getting cached data")
          return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$SetInverse(inv)
        inv
}

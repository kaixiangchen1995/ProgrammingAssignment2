## Put comments here that give an overall description of what your
## functions do

## Function "makeCacheMatrix" creates a special "matrix" 
## object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        ## return a list containing functions and can cache the inverse
    inverse <- NULL
    set <- function(y){
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    set_inverse <- function(matrix_inverse) inverse <<- matrix_inverse 
    get_inverse <- function() inverse
    return(list(set = set, get = get, set_inverse = set_inverse, 
                get_inverse = get_inverse))

}


## Function "cacheSolve" firstly check if the inverse of a special matrix
## have been calculated and cached. If yes, cacheSolve would retrieve the
## inverse; if no, cacheSolve would calculate the inverse of the special 
## matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$get_inverse()
    if(!is.null(inverse)){
        message("getting cache data")
        return(inverse)
    }
    matrix <- x$get()
    inverse <- solve(matrix)
    x$set_inverse(inverse)
    inverse
}

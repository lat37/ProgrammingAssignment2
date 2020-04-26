## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
            inv <- NULL
    set <- function(l){
     ##The demarcation "<<-" assigns and sets a value to a matrix object in a new environment that differs from the current environment.  
        x <<- l
        inv <<- NULL
    }
    get <- function() x
    ##Receives value of matrix and then sets inverse cache value of matrix
    setinv <-function(inverse)  inv <<- inverse
    ##Receives inverse cache value of matrix
    getinv <- function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' 
        ##"X" gives the output to makeCacheMatrix()
        ##Returns the inverse of the original matrix input to makeCacheMatrix()
           inv <-x$getinv
    ##when the inverse has already been calculated
    if (!is.null(inv)){
    ##Calculation derived from cache to skip calculation
        message("receiving cached data")
        return(inv)
    }
    ##if not calculate, the inverse
    mat.data=x$get()
    inv=solve(mat.data,...)
    ##Utilizes the set inverse function in order to set the value of inverse in the cache
    x$setinv(inv)
    
    return(inv)
}

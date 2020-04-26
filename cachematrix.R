## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
            inv <- NULL
    set <- function(l){
        x <<- l
        inv <<- NULL
    }
    get <- function() x
    setinv <-function(inverse)  inv <<- inverse
    getinv <- function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
           inv <-x$getinv
    ##when the inverse has already been calculated
    if (!is.null(inv)){
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

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##1. Set the value of the matrix.
##2. Find the value of the matrix.
##3. Set the inverse of the matrix.
##4. Derive the inverse of the matrix.

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
##"CacheSolve" calcullates the inversse of the special matrix cache function.  The first 
## steps questions whether the inverse has been calculated.  If not, the CacheSolve function
## calculates the invese of the data and sets the inverse matrix in the cache via the setInverse
## function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

    inv <-x$getinv
    
    if (!is.null(inv)){
        message("receiving cached data")
        return(inv)
    }
    mat.data=x$get()
    inv=solve(mat.data,...)
    
    return(inv)
}


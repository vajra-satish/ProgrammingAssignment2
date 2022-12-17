## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The makeCacheMatrix creates a special matrix which can cache its inverse.
## The function carries out the following tasks:
## 1. sets the value fo matrix & initializes the inverse as NULL
## 2. gets the value of matrix
## 3. sets the value of inverse
## 4. gets the value of inverse
makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function() x
        setinv<-function(solve) m<<-solve
        getinv<-function() m
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Write a short comment describing this function

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getinv()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        matrix<-x$get()
        m<-solve(matrix,...)
        x$setinv(m)
        m
}

## these two functions together can compute and cache the inverse of a matrix 
##  this avoids repeated calculations of the inverse

## this creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        s<-NULL
        set <- function(y){
                x<<-y
                s<<-NULL
        }
        get <-function() x
        setsolve <-function(solve) s<<-solve
        getsolve <- function() s
        list(set=set,get=get,
             setsolve=setsolve,
             getsolve=getsolve)
}


## this function computes the inverse of the matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        s<-x$getsolve()
        if(!is.null(s)){
                message("getting cached data")
                return(s)
        }
        data<-x$get()
        s<-solve(data,...)
        x$setsolve(s)
        s
        ## Return a matrix that is the inverse of 'x'
}

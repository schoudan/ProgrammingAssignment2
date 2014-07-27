## Following functions will help to calculate and cache the inverse of a matrix.
## If the inverse has already been calculated (and the matrix has not changed), 
## the function will simply return the inverse from the cache instead of recalcuating it again

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    get<-function() x
    setmatrix<-function(solve) m<<- solve
    getmatrix<-function() m
    list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix function. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve function retrieves the inverse from the cache insead of recomputing it

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m<-x$getmatrix()
    if(!is.null(m)){
      message("getting cached data")
      return(m)
    }
    matrix<-x$get()
    m<-solve(matrix, ...)
    x$setmatrix(m)
    m
}

## Put comments here that give an overall description of what your
## functions do

## Creates a function whose output is a list of functions that sets or
##gets the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse<- NULL
  set<- function(y){
    x<<- y
    inverse<<-NULL
  }
  get<-function() {
    x
  }
  setinverse<- function(inv){
    inverse<<- inv
  }
  
  getinverse<- function(){
    inverse
  }
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## calculates the inverse from the matrix defined in the previous function.
##It checks to see if the inverse has already been calculated.

cacheSolve <- function(x, ...) {
    inv<- x$getinverse()
    if (!is.null(inv)){
      message("getting cached data")
      return(inv)
    }
    data<-x$get()
    inv<-solve(data,...)
    x$setinverse(inv)
    inv
}

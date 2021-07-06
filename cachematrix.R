## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix is a pair of functions that cache the inverse of a matrix

## Write a short comment describing this function
## set            set the value of a matrix
## get            get the value of a matrix
## setInverse     get the cached value (inverse of the matrix)
## getInverse     get the cached value (inverse of the matrix)


makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function(){x}
  setInverse<-function(inverse){inv<<-inverse}
  getInverse<-function(){inv}
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)

}


## Write a short comment describing this function
## Compute and cache the inverse of a matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat<-x$get()
  inv<-solve(mat,...)
  x$setInverse(inv)
  inv
}

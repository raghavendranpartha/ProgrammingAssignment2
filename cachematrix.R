## Put comments here that give an overall description of what your
## functions do
#Pair of functions to calculate and cache the inverse of a matrix for fast computation


## Write a short comment describing this function
#This function creates a special matrix, which is a list of 4 functions that get and set a matrix and 
#get and set the matrix's inverse

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set <- function(y){
    x<<-y
    inv<<-NULL
  }
  get <- function() x
  setInv <- function(inverse) inv<<-inverse
  getInv <- function() inv
  list(set=set,get=get,setInv=setInv,getInv=getInv)
}


## Write a short comment describing this function
# This function returns the inverse of the matrix in the special matrix 'x'. If the matrix had changed (by calling the 
# set function), it calculates the inverse using solve function, if not it returns the cached value
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getInv()
  if(!is.null(inv)){
    message("getting cached inverse")
    return(inv)
  }
  mat<-x$get()
  matInv<-solve(mat, ...)
  x$setInv(matInv)
  matInv
}

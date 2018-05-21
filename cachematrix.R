## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv<<-NULL           ##set the value of the matrix
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function(){x}   ## get the value of the matrix
  setinv<-function(c){inv<<-c}   ## set the value of the inverse of the matrix
  getinv<-function(){inv}  ##get the value of the inverse of the matrix
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
      inv<-x$getinv()   ## get the value of the inverse that is already been stored in the cache
      if(!is.null(inv)){
        message("get data")    ## return cached inverse value of the matrix x
        return(inv)
      }
      data<-x$get()   ## get the value of the matrix
      inv<<-solve(data)  ## calculate the inverse
      x$setinv(inv)   ##store inverse in the cache
      inv
        ## Return a matrix that is the inverse of 'x'
}

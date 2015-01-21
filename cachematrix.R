## the function makeCacheMatrix contains 4 methods. Those methods allow to deal with matrix x, store inverse matrix inv and cache it. 
## By calling the set function the inverse matrix inv is erased and after calculation replace by new  value.

makeCacheMatrix<-function(x=matrix())
{
  inv<-NULL ##init inveres property
  set<-function(y) ##set the matrix x
  {
    x<<-y
    inv<<-NULL
  }
  get<-function() x ##get the matrix x
  setinverse<-function(inverse) inv<<-inverse ##set the inverse matrix of x
  getinverse<-function() inv ##get the inverse matrix of x
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse) ##list of internal methods
}

# Here we receive the parameter "x"
# as a instance of "makeCacheMatrix".
# We first check the value of inverse.

cacheSolve<-function(x,...)
{
  inv<-x$getinverse()
  if(!is.null(inv)) ##checking if inv is null
  {
    message("Getting cached data.")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data) ##inverting the matrix
  s$setinverse(inv)
  inv
}

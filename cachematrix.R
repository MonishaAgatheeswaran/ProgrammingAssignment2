## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Initializing the objects x and cm(cachematrix).
##set function takes the new matrix and assigns it to x
##get function returns the new matrix x
##setinv function sets the inverse of x to cm by using solve function
##getinv returns the inverse matrix cm
##a list set,get,setinv,getinv created for x so that they can be called
makeCacheMatrix <- function(x = matrix()) {
  #cm:cachematrix
  cm<-NULL
  set<-function(y){
    x<<-y
    cm<<-NULL
  }
  get<-function() x
  setinv<-function(solve) cm<<-solve
  getinv<-function() cm
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## Write a short comment describing this function
##getinv assigns the inv to cm
##if it is not null,the cached data is return
##otherwise get fn returns the new matrix and inverse for that is calculated using solve function
##the calculate inverse matrix is set as inverse for x and returned

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  cm<-x$getinv()
  if(!is.null(cm)){
    message("getting cached data")
    return(cm)
  }
  data<-x$get()
  cm<-solve(data, ...)
  x$setinv(cm)
  cm
}

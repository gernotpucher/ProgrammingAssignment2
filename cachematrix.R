## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix results in a list with a function that
# set and gets the value of the matrix as well as of the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
  k<-NULL
  set<-function(y){
  x<<-y
  k<<-NULL
}
get<-function() x
k_set<-function(solve) k<<- solve
k_get<-function() k
list(set=set, get=get,k_set=k,k_get=k_get)
}
# In the cacheSolve function, the matrix is assumed to be invertible. 
cacheSolve <- function(x=matrix(), ...) {
    k<-x$k_get()
    if(!is.null(k)){
      message("getting cached data")
      return(k)
    }
    matrix <- x$get() 
    k<-solve(matrix, ...)
    x$k_calc(k)
    k
}



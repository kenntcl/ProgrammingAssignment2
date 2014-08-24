## Matrix inversion is usually a costly computation and their may be some benefit to caching the inverse of a matrix rather than compute it repeatedly. 

## This function creates a special "matrix" object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
 
# Setter for the matrix
  set<-function(y){
  x<<-y
  m<<-NULL
 }
 
 # Getter for the matrix
  get<-function() x
 
 # Setter for the inverse
 setmatrix<-function(solve) m<<- solve
 
 # Getter for the inverse
 getmatrix<-function() m
 
 # Return the matrix
 list(set=set, get=get,
  setmatrix=setmatrix,
  getmatrix=getmatrix)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## This function assumes that the matrix is always invertible.

cacheSolve <- function(x=matrix(), ...) {
 m<-x$getmatrix()
 
 # If the inverse is already existed, return it
 if(!is.null(m)){
  message("getting cached data")
  return(m)
 }
 
 # If the inverse does not exist, compute it
 matrix<-x$get()
 m<-solve(matrix, ...)
 
 # Cache the inverse
 x$setmatrix(m)
 
 # Return it
 m
}

## MakeCacheMatrix and cacheSolve are functions 
## to make special "matrix" that stores a matrix
## and cache's its inverse.


## makeCacheMatrix creates a special "matric"
## which is really a list conatining functions to:
## 1. set the values of the matrix
## 2. get the values of the matrix
## 3. set the values of the inverse matrix
## 4. get the values of the inverse matrix

makeCacheMatrix<-function(x=matrix()) {
    inv<-NULL  
    setmat<-function(y){x<<-y;inv<<-NULL}
    getmat<-function() x
    setinv<-function(inversemat){inv<<-inversemat}
    getinv<-function() inv
    list(setmat=setmat, getmat=getmat,
         setinv=setinv, getinv=getinv)
}
  



## cacheSolve compute the inverse of a "matrix"
## created witgh the function makeCacheMatrix.
## However, it first checks to see if the inverse
## has already been calculated. 
## If yes, the inverseis returned from the cache. 
## If no, the inverse is computed using solve() 
## and returned. 

cacheSolve<-function(x,...){
  inv<-x$getinv()
  if(!is.null(inv)) {
    message("getting cached inverse matrice")
    return(inv)
  }
  message("computing inverse matrice")
  mat<-x$getmat()
  inv<-solve(mat)
  x$setinv(inv)
  ## Return a matrix that is the inverse of 'x'
  inv
}



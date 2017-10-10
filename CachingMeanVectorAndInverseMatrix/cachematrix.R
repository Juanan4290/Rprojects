## These two functions cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {

  inverse=NULL
  
  get=function() x
  
  set=function(y){
            x <<- y
          }
  
  getInverse=function() inverse
  
  setInverse=function(y){
            inverse<<-y
          }
  
  list(get=get,set=set,getInverse=getInverse,setInverse=setInverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {

 inverse=x$getInverse()
  
          if (!is.null(inverse)){
            message("Getting cached data")
            return(inverse) ## Return a matrix that is the inverse of 'x'
          }
  
  mat=x$get()
  inverse=solve(mat)
  x$setInverse(inverse)
  inverse 
       
}

Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). Your assignment is to write a pair of functions that cache the inverse of a matrix.

Write the following functions:

makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix  <-  function( x  =  matrix ()) {
                 inve<-NULL
                 set <- function(y) {
                                 x <<- y
                                 inve <<- NULL
                         }
                 get <- function() {x}
                 setinverse <- function(inverse){inve<<-inverse}
                 getinverse <- function(){inve}
                 list(set=set, get=get,
                      setinverse=setinverse, 
                      getinverse=getinverse)
}

cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve  <- function ( x , ... ) { 
                inve <- x$getinverse()
                 if(!is.null(inve)){
                        message("getting cached data")
                        return(inve)
                }
                matrix <- x$get()
                inve <- solve(matrix,...)
                x$setinverse(inve)
                inve
}
m1<- makeCacheMatrix(matrix(1:4,nrow = 2,ncol = 2))
m1$get()
m1$getinverse()
cacheSolve(m1)

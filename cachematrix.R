## Description:
##   makeCacheMatrix create an object contain a matrix, which cacheSolve
##   can consume and calculate its inverse (provided the matrix invertible).
##   Made possible by example code provided by course.org from repo rdpeng.
##
## Example:
##    z <- makeCacheMatrix()
##    z$set( c(1,-1,4,6), 2 )
##    cacheSolve(z)


## function: makeCacheMatrix( x=matrix() )
##     initialized with an empty matrix
##
## $set( x, ... )
##    accepts data to populate a matrix 
##    initializes the cached inverse
## $get()
##    returns the matrix
## $setinverse( x )
##    private function, sets the calculated inverse
## $getinverse()
##    returns the calculated inverse

makeCacheMatrix <- function( x = matrix() ) {
   i <- NULL
   set <- function(y, ... ) {
      x <<- matrix( y, ... )
      i <<- NULL
   }
   get <- function() x
   setinverse <- function(solve) i <<- solve
   getinverse <- function() i
   list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## function: cacheSolve( x, ... )
##    accepts a makeCacheMatrix and retrieves
##    any cached value for the inversed matrix.
##    When none exist, it retrieves the data from
##    makeCacheMatrix, calculates the inverse and
#     stores it in makeCacheMatrix using the $set method.
##
##    returns : inverse of the makeCacheMatrix
cacheSolve <- function(x, ...) {
   i <- x$getinverse()
   if(!is.null(i)) {
      message("getting cached data")
      return(i)
   }
   data <- x$get()
   i <- solve(data, ...)
   x$setinverse(i)
   i
}

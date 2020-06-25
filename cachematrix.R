## this function is a make cache matrix.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## this function is a cache solve

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i##returns i as inverse matrix
}
##input and function call:
A<-matrix(c(4,5,6,7),2,2) 
a1<-makeCacheMatrix(A)
cacheSolve(a1)
##output:[,1] [,2]
##[1,] -3.5    3
##[2,]  2.5   -2



## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# Creates matrix object that can cahce inverse
makeCacheMatrix <- function(x = matrix()) {
m <- NULL
set <- function(y){
  x<<- y
  m <<- NULL
}
get <- function() x
setinverse <- function(inverse) m <<- inverse
getinverse <- function() m
list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}




## Write a short comment describing this function
## This function computes the inverse of the special matrix returned by makeCacheMatrix
##If inverse has already been calculated, then cacheSolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)){
    message('getting cached data')
    return(m)
  }
  data <- x$get()
  m <- solve(data)%*%data
  x$setinverse(m)
  m
}

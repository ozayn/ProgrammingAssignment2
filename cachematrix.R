## cachematrix
## 2019-03-26
# makeCacheMatrix and cacheSolve are two functions that are used to create a special object that stores a numeric matrix and caches its inverse matrix.
#####################################################
## makeCacheMatrix 
# creates a special "matrix", which is a list containing a function to 

# 1. set the value of the matrix
# 2. get the value of the matrix
# 2. set the value of the matrix inverse
# 3. get the value of the matrix inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
#####################################################
## cacheSolve
# calculates the inverse of the special "matrix" created with makeCacheMatrix function.
# It first checks to see if the inverse has already been calcualted.
# If so, it gets the matrix inverse from the cache and skips the computation.
# Otherwise, it calculates the matrix inverse of the data 
# and sets the value of the mean in the cache via the setinverse function
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
attr(makeCacheMatrix,"comment")<-"creates a special 'matrix'"
attr(cacheSolve,"comment")<-"Return a matrix that is the inverse of 'x'"
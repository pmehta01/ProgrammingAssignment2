## This file contains functions to cache the inverse of a matrix

## This file contains 2 functions: 1) makeCachematrix, which takes in a special 
## matrix as an argument, and returns a special object that can cache its inverse
## and 2) cacheSolve which computes the inverse of the special matrix returned by makeCachematrix

## MakeCachematrix
## makeCachematrix takes in a matrix object as an argument and returns an object that can
## cache its inverse.  The matrix is assumed to be square and invertible



makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) 
}


## The function CacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cachesolve should retrieve the inverse from the cache. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinverse(inv)
  inv
}



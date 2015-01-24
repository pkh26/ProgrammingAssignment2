## Together the functions cacheSolve and makeCacheMatrix inverts an invertible matrix,
## caches the result, and returns an invertible matrix.

## makeCacheMartix caches the inverse of its invertible matrix argumnet. Is called by cacheSolve.

makeCacheMatrix <- function(mat = matrix()) {
    inv <- NULL
  
    set <- function(y) {
      mat <<- y
      inv <<- NULL
    }
  
    get <- function() mat
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set,get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve test for a change in the nmatrix from makeCacheMatrix, change = true then
## calculate inverse, if false retrive inverse from cache, return inverse.

cacheSolve <- function(fnlist, ...) {
    inv <- fnlist$getinv()
  
    if(!is.null(inv)) {
      message("getting cached matrix inverse")
      return(inv)
    }
    message("calculated matrix inverse")
    data <- fnlist$get()
    inv <- solve(data)
    fnlist$setinv(inv)
    inv
}

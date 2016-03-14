## function makeCacheMatrix(): Returns a special matrix (list) that allows you
## to cache it's inverse to expediantly fetch at a later time.

makeCacheMatrix <- function(x = matrix()) {
  
  set <- function(x) {
    MAT <<- x 
    INV_CACHE <<- NULL # empty cache with new instatiation
  }
  
  get <- function() MAT
  
  setInverse <- function(inv) INV_CACHE <<- inv
  
  getInverse <- function() INV_CACHE
  
  set(x) # Initial instantiation

  list(
    set=set
  , get=get
  , setInverse=setInverse
  , getInverse=getInverse
  )
}


## function cacheSolve(): Will pull value from cache if it exists else call 
## solve on the matrix and cache the results for future use. Finally, it returns
## the inverse of the matrix.

cacheSolve <- function(x, ...) { ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getInverse()
  
  if (!is.null(inv)) {
    return(inv) # return inverse from cache.
  }
  
  inv <- solve(x$get()) # get inverse 
  
  x$setInverse(inv) # cache inverse for future use
  
  inv
}

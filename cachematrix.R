###########################################
## The cacheMatrix functions provides a caching mechanism for calculating 
## inverses of matrices
##
## Example use:
##
## myMatrix <- matrix(1:4, nrow=2, ncol=2)
## myCache <- makeCacheMatrix(myMatrix)
## inv1 <- cacheSolve(myCache) ## calculates and returns the inverse
## inv2 <- cacheSolve(myCache) ## returns the cached inverse
##
###########################################

####
## Returns a matrix cache which can store the matrix itself and its inverse
## 
## args:
##   x - a square matrix
####
makeCacheMatrix <- function(x = matrix()) {
  cachedInverse <- NULL
  
  ## Sets the matrix which inverse should be cached
  ## Throws error if the matrix is not square
  set <- function(aX = matrix()) {
    stopifnot(!is.na(aX), nrow(aX)==ncol(aX))
    x <<- aX
    cachedInverse <<- NULL
  }
  
  ## Returns the matrix in the cache
  get <- function() x
  
  ## Sets the inverse of the matrix in the cache
  setInverse <- function(aInverse) {
    cachedInverse <<- aInverse
  }
  ## Returns the inversed matrix from cache
  getInverse <- function() cachedInverse

  set(x)
  return(list(set=set, get=get, setInverse=setInverse, getInverse=getInverse))
}


####
## Returns the inverse of a matrix, which is stored in a CacheMatrix
## 
## args:
##   aCacheMatrix - a cache created with makeCacheMatrix()
####
cacheSolve <- function(aCacheMatrix=makeCacheMatrix(), ...) {
  ## First check cache
  cachedInverse <- aCacheMatrix$getInverse()
  if(!is.null(cachedInverse)) {
    return(cachedInverse)
  }
  
  ## If not cached, calculate the inverse
  inverse <- solve(aCacheMatrix$get())
  aCacheMatrix$setInverse(inverse)
  return(inverse)
}
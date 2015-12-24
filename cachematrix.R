# makeCacheMatrix is a function that defines a list of methods 
# Its puspose is to store a martix and a cached value of the inverse of the  
# matrix. Contains the following methods: 
# * setMatrix      set the value of a matrix 
# * getMatrix      get the value of a matrix   
# * cacheInverse   get the cahced value (inverse of the matrix) 
# * getInverse     get the cahced value (inverse of the matrix) 
# 

makeCacheMatrix <- function(x = matrix()) {
  mx <- NULL
  set <- function(y) {
    x <<- y
    mx <<- NULL
  }
  get <- function() x
  setInverse <- function(Inverse) mx <<- Inverse
  getInverse <- function() mx
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


# The following function calculates the inverse of a "special" matrix created with  
# makeCacheMatrix 

cacheSolve <- function(x, ... ){
  ## Return a matrix that is the inverse of 'x)'
  mx <- x$getInverse()
  if(!is.null(mx)) {
    message("getting cached inverse matrix")
    return(mx)
  }
  message("inverse is not in memory so the inverse will be computed (if possible)")
  data <- x$get()
  mx <- solve(data, ...)
  x$setInverse(mx)
  
  # return the inverse
  mx
}

## makeCacheMatrix and cacheSolve allow for creation of a matrix wherein the inverse
## once calculated is cached and can be recalled without recalculation

## makeCacheMatrix returns a list that consists of functions to get and set the matrix,
## as well as the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
      Inverse <- NULL
      
      set <- function(y){
            x <<- y
            Inverse <<- NULL
      }
      
      get <- function() x
      
      setInverse <- function(Inv) Inverse <<- Inv
      
      getInverse <- function() Inverse
      
      list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve accepts a list as created by the makeCacheMatrixFunction as an argumentand either
## calculates and returns the inverse of the matrix or returns the cached inverse

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      
      Inv <- x$getInverse()
      
      if(!is.null(Inv)){
            message('Cached inverse returned')
            return(Inv)
      }
      
      mat <- x$get()
      Inv <- solve(mat)
      x$setInverse(Inv)
      Inv
}

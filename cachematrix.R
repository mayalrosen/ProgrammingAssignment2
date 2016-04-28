# These two functions together allow for a matrix to be inverted and then cached. 
# Once cached, the matrix can be retrieved without recomputing. This will happen 
# every time until the matrix is updated and the new matrix will be inverted. 

makeCacheMatrix <- function(x = matrix()) {
    ## makeCacheMatrix is a 'constructor' function 
    ## that when called produces a hash map with 
    ## key -> value pairs where the value is a function.
    ## It takes in matrix, x.

  m <- NULL 

  set <- function(y) {
    x <<- y 
    m <<- NULL 
    
  }
  get <- function() x 
  setInverse  <- function(solve) m <<- solve  
  getInverse <- function() m 
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse) 
  
}


cacheSolve <- function(x, ...) {
    ## cacheSolve is a function that first check whether
    ## an inverted matrix exists in the cache
    ## If cached inverted matrix exists, it returns it.
    ## If it does not exist, it inverts the matrix passed
    ## into makeCacheMatrix and then returns the inverted
    ## matrix.
    m <- x$getInverse()

    if( !is.null(m) ) {
      message("getting cached inverted matrix!")
      return(m)
    }

    message("inverting the matrix because it hasn't been done yet!")
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)

    m
}

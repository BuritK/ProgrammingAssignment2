## Below are two functions that are used to create a special object that... 
## stores a matrix's values and caches its inverse

# The following "makeCacheMatrix" function creates and returns a "special" list containing four functions to: 
# 1. Set (or update) the values of the matrix
# 2. Get the existing values of the matrix
# 3. Set the values of the matrix's inverse
# 4. Get the values of the matrix's inverse
makeCacheMatrix <- function(x = matrix()) {
  inv  <- NULL
  set <- function(y){
          x <<- y
          inv <<- NULL
  }
  get  <- function() x
  setinv  <- function(inverse) inv <<- inverse
  getinv  <- function() inv
  list(set = set, get = get, 
       setinv = setinv,
       getinv = getinv)
}

# The following "cacheSolve" function calcuates the inverse of the list created by the above function. 
# It first checks to see if the inverse has already been calculated. 
# If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the cache via the setinv function. 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv  <- x$getinv()
  if(!is.null(inv)){
    message("Getting Cached Data")
    return(inv)
  }
  data  <- x$get()
  inv  <- solve(data)
  x$setinv(inv)
  inv
}
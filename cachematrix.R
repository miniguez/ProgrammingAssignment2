## Put comments here that give an overall description of what your
## functions do
# How to 
# > x <- matrix(rnorm(10), nrow = 4)          // Create a matrix x
# > cx <- makeCacheMatrix(x)                  // Create our special matrix
# > cx$get()                                  // Return the matrix
# > cacheSolve(cx)                            // Return the inverse
# > cacheSolve(cx)                            // Call again cacheSolve in get the cache inverse


#makeCacheMatrix: return a list of functions (set,get Matrix, set and get inverse matrix)
makeCacheMatrix <- function(x = matrix()) {
  # variable to set the matrix
  inv <- NULL
  
  # function to set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # function to get the matrix
  get <- function() x
  
  # function to set the inverse
  setinv <- function(inverse) inv <<- inverse
  # fucntion to  get the  inverse
  getinv <- function() inv
  
  # Return the matrix 
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}
#cacheSolve: return the inverse of the matrix. If the inverse is already
#calculated , it returns the cached inverse.
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  
  #If the inverse is already calculated, return it
  if (!is.null(inv)) {
    return(inv)
  }
  #Return a matrix that is the inverse of 'x'
  data <- x$get()
  inv <- solve(data, ...)
  
  # Cache the inverse
  x$setinv(inv)
  
  # Return it
  inv
}

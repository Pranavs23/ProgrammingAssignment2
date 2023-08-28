
## Write a short comment describing this function
#the function catcheMatrix  Creates a cache for storing a matrix and its inverse.
#The cache includes methods for setting and retrieving the matrix,as well as setting and retrieving the cached inverse.

makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL # Initialize cached inverse as NULL
  set <- function(y) {  # Setter function for the matrix
    x <<- y  # Assign the matrix to the closure variable
    inv <<- NULL  # Reset the cached inverse
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)

}


## Write a short comment describing this function
#the function cacheSolveCalculates the inverse of a matrix using caching to optimize repeated calculations.
# Checks if the inverse is already cached, and if so, returns the cached result.,If not, it calculates
# the inverse using the solve function, caches it,and returns the calculated inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix_to_invert <- x$get()  # Retrieve matrix to invert
  inv <- solve(matrix_to_invert, ...)  # Calculate inverse
  x$setinverse(inv)  # Cache the calculated inverse
  inv  # Return the calculated inverse
}
my_Matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
my_Matrix$get()
my_Matrix$getinverse()
cacheSolve(my_Matrix)
my_Matrix$getinverse()

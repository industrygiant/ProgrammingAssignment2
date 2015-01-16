## The purpose of these functions is to avoid having to invert a matrix more than once, as it
## can be quite computationally expensive to do so. 

## makeCacheMatrix is a set of functions which caches a matrix and can also cache its inverse when
## set by another function called cacheSolve
makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL #sets inverse to Null upon making the matrix
  
  set <- function(y = matrix){  # This function allows us to manually set the matrix
    x <<- y
    inv <<- NULL
  }
  get <- function() x # function which provides the cached matrix x
  setinv <- function(inverse) inv <<- inverse # will allow cacheSolve to store the inverse it calculates to 'inv' in this function
  getinv <- function() inv # Will provide the matrix inverse 
  list( set = set, get = get, setinv = setinv, getinv = getinv  ) # returns a list of our functions
}


## cacheSolve works in tandem with makeCacheMatrix by either calculating and caching the inverse of a 
## matrix, or providing the cached value if it has already been calculated. 
cacheSolve <- function(x, ...) {

  ## Returns a matrix that is the inverse of 'x'
  
  inv <- x$getinv()    # Checks to see if inverse 'inv' has been calculated already
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)       # If so, we print a message to screen and return the inverse from the cache
  }
  
  #If the inverse has not been calculated we calculate it now and store the result in the cached object x
  matrix <- x$get() # Looks up the matrix in the cache and assigns it to 'matrix' variable
  inv <- solve(matrix) # we invert 'matrix' and assign it to inv
  x$setinv(inv)       # inverse 'inv' is stored in the cache
  inv                 # returns inv
}

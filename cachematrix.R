## When this function is called for a valid non-singularsquare matrix,
## the inverse of the matrix  is calculated and cacheed.
## Next time the martix inverse is recalled it will search in its  
## cache if already calculatednand returns it.
## Example: Lets say you want to find an inverse of a matrix 
##  A <- matrix(c(1,-1,1, -1,2,1,-1,3,4),3,3)
## A <- makeCacheMatrix() associates A to the cachematrix list
## A$set(matrix(c(1,-1,1, -1,2,1,-1,3,4),3,3)) sets the matrix
## cacheSolve(A) calculates and saves the inverse.
## next time cacheSolve(A) is run, it will indicate the it is
## getting the value from Cache

## This code snippet saves the matrix A and its inverse in a list
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
}



## The Solve functions below determines the Inverse of matrix.
## cacheSolve (x) uses the x$getsolve to see if the inverse already exists
## via m. If m is valid it returns m. Else, it calculates via solve(x)
## and saves the value m for x"inverse" in the list.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

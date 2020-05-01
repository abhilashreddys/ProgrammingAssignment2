## https://github.com/abhilashreddys/ProgrammingAssignment2

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                                  ## inv will hold inverse of the matrix;(default = NULL)
  set <- function(y) {                         ## set function - if there is a new matrix
    x <<- y                                    ## assign the value of new matrix in parent environment
    inv <<- NULL                               ## reset inv to NULL
  }
  get <- function() x                          ## function  get - returns value of the matrix argument
  setinv <- function(inverse) inv <<- inverse  ## assigns value of inv in parent environment
  getinv <- function() inv                     ## gets the value of inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()       		  ## gets inverse matrix
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...) 		  ## if inv == NULL, solving 'x' for inverse matrix
  x$setinv(inv)           		  ## setting inverse matrix
  inv           
}

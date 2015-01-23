## makeCacheMatrix creates a vector of functions required, for a given input matrix x
makeCacheMatrix <- function(x=matrix()) {
  inv <- NULL               ## inputs a null value into x
  set <- function(y) {      ## sets an input matrix y into x in the global environment
    x <<- y
    inv <<- NULL            ## and nulls inv in the global environment
  }
  get <- function() x       ## the get function just returns the value of x
  setInv <- function(solution) inv <<- solution   ## setInv inputs the solution (inverse matrix) into inv in the global environment
  getInv <- function() inv  ## the getInv function just returns te value of inv, the solution (inverse matrix)
  list(set = set,           ## makeCacheMatrix outputs a vector with the above functions
       get = get,
       setInv = setInv,
       getInv = getInv)
}

## use makeCacheMatrix as follows (for an example 2 by 2 matrix with values 1:4):
## myMatrix <- makeCacheMatrix(matrix(1:4,2,2))


## cacheSolve returns the inverse of a matrix, but searches first for a cached solution
cacheSolve <- function(x, ...) {
  inv <- x$getInv()     ## calls the getInv function from makeCacheMatrix
                        ## this just returns the inverse of our chosen matrix (if already calculated)
  if (!is.null(inv)) {  ## if this is already populated (i.e. previously cached), the inverse matrix is returned, with a message
    message("getting cached data") 
    return(inv)
  }
  data <- x$get()       ## otherwise our matrix chosen in makeCacheMatrix is input into data
  inv <- solve(data, ...)  ## and the inverse of data is output into inv
  x$setInv(inv)         ## setInv is then called to declare inv into the global environment, caching it for future use
  inv                   ## inv is then returned
}

## cacheSolve is used as follows:
## cacheSolve(myMatrix)
## the first time this is run, no cached solution exists, and the inverse matrix is calculated and returned
## the next time it is run, the inverse is found in the cache and returned (with the confirmation message) without being recalculated

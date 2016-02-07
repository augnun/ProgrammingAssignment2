# Augusto Cesar Ribeiro Nunes


# I chose to use the Roxygen ( http://roxygen.org/ ) system for commenting
# my code. It is really useful.


#' Title makeCacheMatrix()
#'
#' @param x
#'
#' @return list object containing nested functions set(), get(), setInverse()
#' and getInverse()
#' @export
#'
#' @examples
#' > a = cbind(c(2,3), c(9/5,1))
#' > A = makeCacheMatrix(a)
#' > A$get()
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() {
    x
  }
  
  setInverse <- function(matrix) {
    inverse <<- matrix
  }
  
  getInverse <- function() {
    inverse
  }
  
  list(
    set = set,
    get = get,
    setInverse = setInverse,
    getInverse = getInverse
  )
}


## Write a short comment describing this function

#' Title cacheSolve()
#'
#' @param x
#' @param ...
#'
#' @return the inverse of a matrix
#' @export
#'
#' @examples
#' cacheSolve(A)
cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  
  
  # Condition: is there a cached answer to the inverse matrix?
  # If TRUE (not null), retrieve it
  if (!is.null(inverse)) {
    message("Retrieving the cached matrix")
    return(inverse)
  }
  
  # Otherwise, calculate the inverse of the matrix
  data <- x$get()
  
  inverse <- solve(data)
  
  x$setInverse(inverse)
  
  # and return it
  inverse
  
}
